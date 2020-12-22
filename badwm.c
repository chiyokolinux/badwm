/*******************************************************************************
 *                                                                             *
 * badwm - A simple, non-bloated, tiling window manager, based on tinywm       *
 * Copyright (C) 2019-2020 Jonas Jaguar <jonasjaguar@jagudev.net>              *
 *                                                                             *
 * This program is free software: you can redistribute it and/or modify it     *
 * under the terms of the GNU General Public License as published by the Free  *
 * Software Foundation, either version 3 of the License, or (at your option)   *
 * any later version.                                                          *
 *                                                                             *
 * This program is distributed in the hope that it will be useful, but WITHOUT *
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or       *
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for   *
 * more details.                                                               *
 *                                                                             *
 * You should have received a copy of the GNU General Public License along     *
 * with this program.  If not, see <https://www.gnu.org/licenses/>.            *
 *                                                                             *
 *******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <err.h>
#include <stdarg.h>
#include <unistd.h>
#include <signal.h>
#include <pthread.h>
#include <time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include <X11/XKBlib.h>
#include <X11/Xproto.h>
#include <X11/Xatom.h>

#define LENGTH(x)       (sizeof(x)/sizeof(*x))
#define CLEANMASK(mask) (mask & ~(numlockmask | LockMask))
#define ISFFT(c)        (c->isfull)
#define ROOTMASK        SubstructureRedirectMask|ButtonPressMask|SubstructureNotifyMask|PropertyChangeMask
#define ITOA(n) my_itoa((char [3]) { 0 }, (n) )

enum { WM_PROTOCOLS, WM_DELETE_WINDOW, WM_COUNT };
enum { NET_SUPPORTED, NET_FULLSCREEN, NET_WM_STATE, NET_ACTIVE, NET_WM_NAME, NET_COUNT };

typedef union {
    const char** com;
    const int i;
    const void *v;
} Arg;

typedef struct {
    unsigned int mod;
    KeySym keysym;
    void (*func)(const Arg *);
    const Arg arg;
} Key;

typedef struct {
    const char *class;
    const int desktop;
} AppRule;

typedef struct Client {
    struct Client *next;
    Bool isurgn, isfull;
    Window win;
} Client;

typedef struct {
    int masz, sasz;
    Client *head, *prev, *curr;
} Desktop;

static void change_desktop(const Arg *arg);
static void client_to_desktop(const Arg *arg);
static void close_win();
static void move_down();
static void move_up();
static void next_win();
static void prev_win();
static void quit();
static void spawn(const Arg *arg);
static void swap_master();
static void toggle_fullscreen();

static Client* addwindow(Window w, Desktop *d);
static void cleanup(void);
static void deletewindow(Window w);
static void focus(Client *c, Desktop *d);
static unsigned long getcolor(const char* color, const int screen);
static void removeclient(Client *c, Desktop *d);
static void setfullscreen(Client *c, Desktop *d, Bool fullscrn);
static void sigchld(int sig);
static Bool wintoclient(Window w, Client **c, Desktop **d);
static Client* prevclient(Client *c, Desktop *d);

static void initwm(void);
static void runwm(void);

static int xerror(Display *dis, XErrorEvent *ee);
static int xerrorstart(Display *dis, XErrorEvent *ee);

static void tile(Desktop *d);
static void stack(int x, int y, int w, int h, const Desktop *d);
static void fullscreen(int x, int y, int w, int h, const Desktop *d);

static char* my_itoa(char *dest, int i);
static Bool deskhasurgn(Desktop *d);
static void loadfont();
static void initbar();
static void *runbar(void *arg);
static void renderbar();

static void propertynotify(XEvent *e);
static void unmapnotify(XEvent *e);
static void keypress(XEvent *e);
static void maprequest(XEvent *e);
static void focusin(XEvent *e);
static void destroynotify(XEvent *e);
static void configurerequest(XEvent *e);
static void enternotify(XEvent *e);

static void grabkeys(void);

#include "config.h"

static Bool running = True, show = True;
static int wh, ww, currdeskidx, prevdeskidx, retval;
static unsigned int numlockmask, win_unfocus, win_focus, win_focus_urgn, win_unfocus_urgn, bgcol, fgcol;
static Display *dis;
static Window root, bar;
static Atom wmatoms[WM_COUNT];
static Atom netatoms[NET_COUNT];
static Atom utf8_atom_type;
static Desktop desktops[4];
static Pixmap pm;
static GC gc;

static void (*events[LASTEvent])(XEvent *e) = {
    [KeyPress]         = keypress,         [EnterNotify]    = enternotify,
    [MapRequest]       = maprequest,       [DestroyNotify]  = destroynotify,
    [UnmapNotify]      = unmapnotify,      [PropertyNotify] = propertynotify,
    [ConfigureRequest] = configurerequest, [FocusIn]        = focusin,
};

Client* addwindow(Window w, Desktop *d) {
    Client *c = NULL, *t = prevclient(d->head, d);
    if (!(c = (Client *)calloc(1, sizeof(Client)))) err(EXIT_FAILURE, "cannot allocate client");
    if (!d->head) d->head = c;
    else { c->next = d->head; d->head = c; }

    XSelectInput(dis, (c->win = w), PropertyChangeMask|FocusChangeMask|(FOLLOW_MOUSE?EnterWindowMask:0));
    renderbar();
    return c;
}

void change_desktop(const Arg *arg) {
    if (arg->i == currdeskidx || arg->i < 0 || arg->i >= 4) return;
    Desktop *d = &desktops[(prevdeskidx = currdeskidx)], *n = &desktops[(currdeskidx = arg->i)];
    if (n->curr) XMapWindow(dis, n->curr->win);
    for (Client *c = n->head; c; c = c->next) XMapWindow(dis, c->win);
    XChangeWindowAttributes(dis, root, CWEventMask, &(XSetWindowAttributes){.do_not_propagate_mask = SubstructureNotifyMask});
    for (Client *c = d->head; c; c = c->next) if (c != d->curr) XUnmapWindow(dis, c->win);
    if (d->curr) XUnmapWindow(dis, d->curr->win);
    XChangeWindowAttributes(dis, root, CWEventMask, &(XSetWindowAttributes){.event_mask = ROOTMASK});
    if (n->head) { tile(n); focus(n->curr, n); }
    renderbar();
}

void cleanup(void) {
    Window root_return, parent_return, *children;
    unsigned int nchildren;
    XFreeGC(dis, gc);
    XFreePixmap(dis, pm);
    XDestroyWindow(dis, bar);

    XUngrabKey(dis, AnyKey, AnyModifier, root);
    XQueryTree(dis, root, &root_return, &parent_return, &children, &nchildren);
    for (unsigned int i = 0; i < nchildren; i++) deletewindow(children[i]);
    if (children) XFree(children);
    XSync(dis, False);
}

void client_to_desktop(const Arg *arg) {
    if (arg->i == currdeskidx || arg->i < 0 || arg->i >= 4 || !desktops[currdeskidx].curr) return;
    Desktop *d = &desktops[currdeskidx], *n = &desktops[arg->i];
    Client *c = d->curr, *p = prevclient(d->curr, d), *l = prevclient(n->head, n);

    if (d->head == c || !p) d->head = c->next; else p->next = c->next;
    c->next = NULL;
    XChangeWindowAttributes(dis, root, CWEventMask, &(XSetWindowAttributes){.do_not_propagate_mask = SubstructureNotifyMask});
    if (XUnmapWindow(dis, c->win)) focus(d->prev, d);
    XChangeWindowAttributes(dis, root, CWEventMask, &(XSetWindowAttributes){.event_mask = ROOTMASK});
    if (d->head && !d->head->next) tile(d);

    focus(l ? (l->next = c):n->head ? (n->head->next = c):(n->head = c), n);

    change_desktop(arg);
    renderbar();
}

void configurerequest(XEvent *e) {
    XConfigureRequestEvent *ev = &e->xconfigurerequest;
    XWindowChanges wc = { ev->x, ev->y,  ev->width, ev->height, ev->border_width, ev->above, ev->detail };
    if (XConfigureWindow(dis, ev->window, ev->value_mask, &wc)) XSync(dis, False);
    Desktop *d = NULL; Client *c = NULL;
    if (wintoclient(ev->window, &c, &d)) tile(d);
    renderbar();
}

void deletewindow(Window w) {
    XEvent ev = { .type = ClientMessage };
    ev.xclient.window = w;
    ev.xclient.format = 32;
    ev.xclient.message_type = wmatoms[WM_PROTOCOLS];
    ev.xclient.data.l[0]    = wmatoms[WM_DELETE_WINDOW];
    ev.xclient.data.l[1]    = CurrentTime;
    XSendEvent(dis, w, False, NoEventMask, &ev);
    renderbar();
}

void destroynotify(XEvent *e) {
    Desktop *d = NULL; Client *c = NULL;
    if (wintoclient(e->xdestroywindow.window, &c, &d)) removeclient(c, d);
    renderbar();
}

void enternotify(XEvent *e) {
    Desktop *d = NULL; Client *c = NULL, *p = NULL;

    if ((e->xcrossing.mode != NotifyNormal && e->xcrossing.detail == NotifyInferior)
        || !wintoclient(e->xcrossing.window, &c, &d) || e->xcrossing.window == d->curr->win) return;

    if ((p = d->prev))
        XChangeWindowAttributes(dis, p->win, CWEventMask, &(XSetWindowAttributes){.do_not_propagate_mask = EnterWindowMask});
    focus(c, d);
    if (p) XChangeWindowAttributes(dis, p->win, CWEventMask, &(XSetWindowAttributes){.event_mask = EnterWindowMask});
    renderbar();
}

void focus(Client *c, Desktop *d) {
    if (!d->head || !c) {
        d->curr = d->prev = NULL;
        return;
    } else if (d->prev == c && d->curr != c->next) { d->prev = prevclient((d->curr = c), d);
    } else if (d->curr != c) { d->prev = d->curr; d->curr = c; }

    int n = 0, fl = 0, ft = 0;
    for (c = d->head; c; c = c->next, ++n) if (ISFFT(c)) { fl++; if (!c->isfull) ft++; }
    Window w[n];
    w[ft] = d->curr->win;
    for (fl += !ISFFT(d->curr) ? 1:0, c = d->head; c; c = c->next) {
        XSetWindowBorder(dis, c->win, c == d->curr ? win_focus:win_unfocus);
        XSetWindowBorderWidth(dis, c->win, c->isfull || (!ISFFT(c) &&
            (d->head->isfull || !d->head->next)) ? 0:BORDER_SIZE);
        if (c != d->curr) w[c->isfull ? --fl:ISFFT(c) ? --ft:--n] = c->win;
    }
    XRestackWindows(dis, w, LENGTH(w));

    XSetInputFocus(dis, d->curr->win, RevertToPointerRoot, CurrentTime);
    XSync(dis, False);
    renderbar();
}

void focusin(XEvent *e) {
    Desktop *d = &desktops[currdeskidx];
    if (d->curr && d->curr->win != e->xfocus.window) focus(d->curr, d);
}

void grabkeys(void) {
    KeyCode code;
    XUngrabKey(dis, AnyKey, AnyModifier, root);
    unsigned int k, m, modifiers[] = { 0, LockMask, numlockmask, numlockmask|LockMask };

    for (k = 0, m = 0; k < LENGTH(keys); k++, m = 0)
        while ((code = XKeysymToKeycode(dis, keys[k].keysym)) && m < LENGTH(modifiers))
            XGrabKey(dis, code, keys[k].mod|modifiers[m++], root, True, GrabModeAsync, GrabModeAsync);
}

void keypress(XEvent *e) {
    KeySym keysym = XkbKeycodeToKeysym(dis, e->xkey.keycode, 0, 0);
    for (unsigned int i = 0; i < LENGTH(keys); i++)
        if (keysym == keys[i].keysym && CLEANMASK(keys[i].mod) == CLEANMASK(e->xkey.state))
            if (keys[i].func) keys[i].func(&keys[i].arg);
}

void close_win(void) {
    Desktop *d = &desktops[currdeskidx];
    if (!d->curr) return;

    Atom *prot = NULL; int n = -1;
    if (XGetWMProtocols(dis, d->curr->win, &prot, &n))
        while(--n >= 0 && prot[n] != wmatoms[WM_DELETE_WINDOW]);
    if (n < 0) { XKillClient(dis, d->curr->win); removeclient(d->curr, d); }
    else deletewindow(d->curr->win);
    if (prot) XFree(prot);
    renderbar();
}

void maprequest(XEvent *e) {
    Desktop *d = NULL; Client *c = NULL;
    Window w = e->xmaprequest.window;
    XWindowAttributes wa = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    if (wintoclient(w, &c, &d) || (XGetWindowAttributes(dis, w, &wa) && wa.override_redirect)) return;

    XClassHint ch = {0, 0};
    Bool follow = False;
    int newdsk = currdeskidx;

    if (XGetClassHint(dis, w, &ch)) for (unsigned int i = 0; i < LENGTH(rules); i++)
        if (strstr(ch.res_class, rules[i].class) || strstr(ch.res_name, rules[i].class)) {
            if (rules[i].desktop >= 0 && rules[i].desktop < 4) newdsk = rules[i].desktop;
            follow = True;
            break;
        }
    if (ch.res_class) XFree(ch.res_class);
    if (ch.res_name) XFree(ch.res_name);

    c = addwindow(w, (d = &desktops[newdsk]));

    int i; unsigned long l; unsigned char *state = NULL; Atom a;
    if (state) XFree(state);

    if (currdeskidx == newdsk) { if (!ISFFT(c)) tile(d); XMapWindow(dis, c->win); }
    else if (follow) change_desktop(&(Arg){.i = newdsk});
    focus(c, d);
}

void move_up(void) {
    Desktop *d = &desktops[currdeskidx];
    if (!d->curr || !d->head->next) return;
    Client *pp = NULL, *p = prevclient(d->curr, d);
    if (p->next) for (pp = d->head; pp && pp->next != p; pp = pp->next);
    if (pp) pp->next = d->curr; else d->head = (d->curr == d->head) ? d->curr->next:d->curr;
    p->next = (d->curr->next == d->head) ? d->curr:d->curr->next;
    d->curr->next = (d->curr->next == d->head) ? NULL:p;
    tile(d);
    renderbar();
}

void move_down(void) {
    Desktop *d = &desktops[currdeskidx];
    if (!d->curr || !d->head->next) return;
    Client *p = prevclient(d->curr, d), *n = (d->curr->next) ? d->curr->next:d->head;
    if (d->curr == d->head) d->head = n; else p->next = d->curr->next;
    d->curr->next = (d->curr->next) ? n->next:n;
    if (d->curr->next == n->next) n->next = d->curr; else d->head = d->curr;
    tile(d);
    renderbar();
}

void next_win(void) {
    Desktop *d = &desktops[currdeskidx];
    if (d->curr && d->head->next) focus(d->curr->next ? d->curr->next:d->head, d);
    renderbar();
}

void prev_win(void) {
    Desktop *d = &desktops[currdeskidx];
    if (d->curr && d->head->next) focus(prevclient(d->curr, d), d);
    renderbar();
}

void propertynotify(XEvent *e) {
    Desktop *d = NULL; Client *c = NULL;
    if (e->xproperty.atom != XA_WM_HINTS || !wintoclient(e->xproperty.window, &c, &d)) return;

    XWMHints *wmh = XGetWMHints(dis, c->win);
    c->isurgn = (c != desktops[currdeskidx].curr && wmh && (wmh->flags & XUrgencyHint));
    if (c->isurgn) {
        XSetWindowBorder(dis, c->win, c == d->curr ? win_focus_urgn:win_unfocus_urgn);
    }

    if (wmh) XFree(wmh);
    renderbar();
}

void quit() {
    retval = 0;
    running = False;
}

void removeclient(Client *c, Desktop *d) {
    Client **p = NULL;
    for (p = &d->head; *p && (*p != c); p = &(*p)->next);
    if (!*p) return; else *p = c->next;
    if (c == d->prev && !(d->prev = prevclient(d->curr, d))) d->prev = d->head;
    if (c == d->curr || (d->head && !d->head->next)) focus(d->prev, d);
    tile(d);
    free(c);
    renderbar();
}

void setfullscreen(Client *c, Desktop *d, Bool fullscrn) {
    c->isfull = fullscrn;
    if (fullscrn) XMoveResizeWindow(dis, c->win, 0, 0, ww, wh + PANEL_HEIGHT);
    else tile(d);
    XSetWindowBorderWidth(dis, c->win, (c->isfull || !d->head->next ? 0:BORDER_SIZE));
    renderbar();
}

void toggle_fullscreen() {
    Desktop *d = &desktops[currdeskidx];
    Client *c = d->head;
    if (!d->head) return;
    setfullscreen(c, d, !c->isfull);
}

void initwm(void) {
    sigchld(0);

    const int screen = DefaultScreen(dis);
    root = RootWindow(dis, screen);

    ww = XDisplayWidth(dis, screen);
    wh = XDisplayHeight(dis, screen) - PANEL_HEIGHT;

    for (unsigned int d = 0; d < 4; d++)
        desktops[d] = (Desktop){ };

    win_focus = getcolor(FOCUS, screen);
    win_unfocus = getcolor(UNFOCUS, screen);
    win_focus_urgn = getcolor(URGENTFOCUS, screen);
    win_unfocus_urgn = getcolor(URGENTUNFOCUS, screen);
    bgcol = getcolor(BACKGROUNDCOL, screen);
    fgcol = getcolor(FOREGROUNDCOL, screen);

    XModifierKeymap *modmap = XGetModifierMapping(dis);
    for (int k = 0; k < 8; k++) for (int j = 0; j < modmap->max_keypermod; j++)
        if (modmap->modifiermap[modmap->max_keypermod*k + j] == XKeysymToKeycode(dis, XK_Num_Lock))
            numlockmask = (1 << k);
    XFreeModifiermap(modmap);

    wmatoms[WM_PROTOCOLS]      = XInternAtom(dis, "WM_PROTOCOLS",     False);
    wmatoms[WM_DELETE_WINDOW]  = XInternAtom(dis, "WM_DELETE_WINDOW", False);
    netatoms[NET_SUPPORTED]    = XInternAtom(dis, "_NET_SUPPORTED",   False);
    netatoms[NET_WM_STATE]     = XInternAtom(dis, "_NET_WM_STATE",    False);
    netatoms[NET_WM_NAME]      = XInternAtom(dis, "_NET_WM_NAME",     False);
    netatoms[NET_ACTIVE]       = XInternAtom(dis, "_NET_ACTIVE_WINDOW",       False);
    netatoms[NET_FULLSCREEN]   = XInternAtom(dis, "_NET_WM_STATE_FULLSCREEN", False);

    utf8_atom_type    = XInternAtom(dis, "UTF8_STRING", False);

    /* set _NET_WM_NAME (for pfetch, neofetch, ufetch, etc...) */
    XChangeProperty(dis, root, netatoms[NET_WM_NAME], utf8_atom_type, 8, PropModeReplace, "badwm", 6);
    /* tell the X Server that we support all the netatoms we have in netatoms[] */
    XChangeProperty(dis, root, netatoms[NET_SUPPORTED], XA_ATOM, 32, PropModeReplace, (unsigned char *)netatoms, NET_COUNT);

    XSetErrorHandler(xerrorstart);
    XSelectInput(dis, root, ROOTMASK);
    XSync(dis, False);
    XSetErrorHandler(xerror);
    XSync(dis, False);

    grabkeys();
    change_desktop(&(Arg){.i = 0});

    XSetWindowAttributes wa = { .background_pixel = bgcol, .override_redirect = 1, .event_mask = ExposureMask, };
    bar = XCreateWindow(dis, root, 0,
            (TOP_PANEL) ? 0 : wh, ww, PANEL_HEIGHT, 1,
            CopyFromParent, InputOutput, CopyFromParent,
            CWBackPixel | CWOverrideRedirect | CWEventMask, &wa);
    XMapWindow(dis, bar);
    XSetWindowBorderWidth(dis, bar, 0);

    XGCValues gcv = { .graphics_exposures = 0, };
    gc = XCreateGC(dis, root, GCGraphicsExposures, &gcv);
    pm = XCreatePixmap(dis, bar, ww, PANEL_HEIGHT, DefaultDepth(dis,screen));
}

void runwm(void) {
    XEvent ev;
#ifdef BADWM_PANEL
    while(running) {
        while (!XCheckMaskEvent(dis, -1, &ev)) { usleep(PANEL_INTERVAL); }
        if (events[ev.type]) events[ev.type](&ev);
    }
#else
    while(running && !XNextEvent(dis, &ev)) if (events[ev.type]) events[ev.type](&ev);
#endif
}

void swap_master(void) {
    Desktop *d = &desktops[currdeskidx];
    if (!d->curr || !d->head->next) return;
    if (d->curr == d->head) move_down();
    else while (d->curr != d->head) move_up();
    focus(d->head, d);
}

unsigned long getcolor(const char* color, const int screen) {
    XColor c; Colormap map = DefaultColormap(dis, screen);
    if (!XAllocNamedColor(dis, map, color, &c, &c)) err(EXIT_FAILURE, "cannot allocate color");
    return c.pixel;
}

void sigchld(__attribute__((unused)) int sig) {
    if (signal(SIGCHLD, sigchld) != SIG_ERR) while(0 < waitpid(-1, NULL, WNOHANG));
    else err(EXIT_FAILURE, "cannot install SIGCHLD handler");
}

Client* prevclient(Client *c, Desktop *d) {
    Client *p = NULL;
    if (c && d->head && d->head->next) for (p = d->head; p->next && p->next != c; p = p->next);
    return p;
}

void spawn(const Arg *arg) {
    if (fork()) return;
    if (dis) close(ConnectionNumber(dis));
    setsid();
    execvp((char*)arg->com[0], (char**)arg->com);
    err(EXIT_SUCCESS, "execvp %s", (char *)arg->com[0]);
    renderbar();
}

void tile(Desktop *d) {
    if (!d->head) return;
    if (d->head->next && !d->head->isfull) {
        stack(0, TOP_PANEL ? PANEL_HEIGHT:0,
              ww, wh + 0, d);
    } else {
        fullscreen(0, TOP_PANEL ? PANEL_HEIGHT:0,
                   ww, wh + 0, d);
    }
    renderbar();
}

void fullscreen(int x, int y, int w, int h, const Desktop *d) {
    for (Client *c = d->head; c; c = c->next) if (!ISFFT(c)) XMoveResizeWindow(dis, c->win, x, y, w, h);
}

void stack(int x, int y, int w, int h, const Desktop *d) {
    Client *c = NULL, *t = NULL; Bool b = BSTACK;
    int n = 0, p = 0, z = (b ? w:h), ma = (b ? h:w) * MASTER_SIZE + d->masz;

    for (t = d->head; t; t = t->next) if (!ISFFT(t)) { if (c) ++n; else c = t; }

    if (c && !n) XMoveResizeWindow(dis, c->win, x + GAPS, y + GAPS,
                w - 2*(BORDER_SIZE + GAPS), h - 2*(BORDER_SIZE + GAPS));
    if (!c || !n) return; else if (n > 1) { p = (z - d->sasz)%n + d->sasz; z = (z - d->sasz)/n; }

    if (b) XMoveResizeWindow(dis, c->win, x + GAPS, y + GAPS,
        w - 2*(BORDER_SIZE + GAPS), ma - 2*(BORDER_SIZE + GAPS));
    else   XMoveResizeWindow(dis, c->win, x + GAPS, y + GAPS,
        ma - 2*(BORDER_SIZE + GAPS), h - 2*(BORDER_SIZE + GAPS));

    for (c = c->next; c && ISFFT(c); c = c->next);
    int ch = z - 2*BORDER_SIZE - GAPS, cw = (b ? h:w) - 2*BORDER_SIZE - ma - GAPS;
    if (b) XMoveResizeWindow(dis, c->win, x += GAPS, y += ma, ch - GAPS + p, cw);
    else   XMoveResizeWindow(dis, c->win, x += ma, y += GAPS, cw, ch - GAPS + p);

    for (b ? (x += z+p-GAPS):(y += z+p-GAPS), c = c->next; c; c = c->next) {
        if (ISFFT(c)) continue;
        if (b) { XMoveResizeWindow(dis, c->win, x, y, ch, cw); x += z; }
        else   { XMoveResizeWindow(dis, c->win, x, y, cw, ch); y += z; }
    }
}

void unmapnotify(XEvent *e) {
    Desktop *d = NULL; Client *c = NULL;
    if (wintoclient(e->xunmap.window, &c, &d)) removeclient(c, d);
}

Bool wintoclient(Window w, Client **c, Desktop **d) {
    for (int i = 0; i < 4 && !*c; i++)
        for (*d = &desktops[i], *c = (*d)->head; *c && (*c)->win != w; *c = (*c)->next);
    return (*c != NULL);
}

Bool deskhasurgn(Desktop *d) {
    Bool urgent = False;
    for (Client *c = d->head; c; urgent |= c->isurgn, c = c->next);
    return urgent;
}

void loadfont() {
    XFontStruct *font = XLoadQueryFont (dis, PANELFONT);
    if (!font) {
        fprintf(stderr, "ERROR: Unable to load font %s: using fixed\n", PANELFONT);
        font = XLoadQueryFont(dis, "fixed");
    }
    XSetFont(dis, gc, font->fid);
}

char *my_itoa(char *dest, int i) {
    sprintf(dest, "%d", i);
    return dest;
}

void *runbar(void *arg) {
    pthread_detach(pthread_self());
    loadfont();
    while (running) {
        renderbar();
        usleep(PANEL_INTERVAL * 1000l);
    }
    return 0;
}

void initbar() {
    pthread_t thread_id;
    if (pthread_create(&thread_id, NULL, runbar, NULL) != 0) {
        fprintf(stderr, "ERROR: Unable to spawn bar thread!\n");
        exit(EXIT_FAILURE);
    }
}

void renderbar() {
    XSetForeground(dis, gc, bgcol);
    XFillRectangle(dis, pm, gc, 0, 0, ww, PANEL_HEIGHT);

    for (unsigned int i = 0; i < 4; i++) {
        if (i == currdeskidx || deskhasurgn(&desktops[i])) {
            XSetForeground(dis, gc, deskhasurgn(&desktops[i]) ? ((i == currdeskidx) ? win_focus_urgn : win_unfocus_urgn) : win_focus);
            XFillRectangle(dis, pm, gc, i * PANEL_HEIGHT, 0, PANEL_HEIGHT, PANEL_HEIGHT);
        } else {
            XSetForeground(dis, gc, win_unfocus);
            XFillRectangle(dis, pm, gc, i * PANEL_HEIGHT, 0, PANEL_HEIGHT, PANEL_HEIGHT);
        }
        XSetForeground(dis, gc, fgcol);
        XDrawString(dis, pm, gc, i * PANEL_HEIGHT + PANELSTARTOFST, PANEL_HEIGHT - PANELSTARTOFST, ITOA(i+1), strlen(ITOA(i+1)));
    }

#ifdef BADWM_PANEL
    XSetForeground(dis, gc, win_unfocus);
    XFillRectangle(dis, pm, gc, ww - (9 * PANEL_HEIGHT) - PANELSTARTOFST, 0, PANELSTARTOFST + (9 * PANEL_HEIGHT), PANEL_HEIGHT);

    time_t timer;
    char buffer[20];
    struct tm* tm_info;
    timer = time(NULL);
    tm_info = localtime(&timer);
    strftime(buffer, 22, "%H:%M:%S %d.%m.%Y", tm_info);
    XSetForeground(dis, gc, fgcol);
    XDrawString(dis, pm, gc, ww - (9 * PANEL_HEIGHT), PANEL_HEIGHT - PANELSTARTOFST, buffer, 19);

    XLockDisplay(dis);
    XCopyArea(dis, pm, bar, gc, 0, 0, ww, PANEL_HEIGHT, 0, 0);
    XSync(dis, False);
    XUnlockDisplay(dis);
#endif
}

int xerror(__attribute__((unused)) Display *dis, XErrorEvent *ee) {
    if ((ee->error_code == BadAccess   && (ee->request_code == X_GrabKey
                                       ||  ee->request_code == X_GrabButton))
    || (ee->error_code  == BadMatch    && (ee->request_code == X_SetInputFocus
                                       ||  ee->request_code == X_ConfigureWindow))
    || (ee->error_code  == BadDrawable && (ee->request_code == X_PolyFillRectangle
    || ee->request_code == X_CopyArea  ||  ee->request_code == X_PolySegment
                                       ||  ee->request_code == X_PolyText8))
    || ee->error_code   == BadWindow) return 0;
    err(EXIT_FAILURE, "XError: Request: %d Code: %d", ee->request_code, ee->error_code);
}

int xerrorstart(__attribute__((unused)) Display *dis, __attribute__((unused)) XErrorEvent *ee) {
    errx(EXIT_FAILURE, "CRITICAL: Another window manager is already running!");
}

int main(int argc, char const *argv[]) {
    if (argc == 2 && !strncmp(argv[1], "-v", 3))
        errx(EXIT_SUCCESS, "badwm - The small, fast window manager");
#ifdef BADWM_PANEL
    XInitThreads();
#endif
    if(!(dis = XOpenDisplay(NULL))) errx(EXIT_FAILURE, "CRITICAL: Could not open X Display - terminating!");
    initwm();
#ifdef BADWM_PANEL
    initbar();
#endif
    loadfont();
    runwm();
    cleanup();
    XCloseDisplay(dis);
    return retval;
}
