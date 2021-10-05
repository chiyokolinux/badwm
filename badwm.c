/*******************************************************************************
 *                                                                             *
 * badwm - A simple, non-bloated, tiling window manager, based on tinywm       *
 * Copyright (C) 2019-2021 Emily <elishikawa@jagudev.net>                      *
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
#define ISFFT(c)        (c->isfull || c->isfloat || c->istrans)
#define ROOTMASK        SubstructureRedirectMask|ButtonPressMask|SubstructureNotifyMask|PropertyChangeMask
#define ITOA(n)         my_itoa((char [3]) { 0 }, (n) )

enum { WM_PROTOCOLS, WM_DELETE_WINDOW, WM_COUNT };
enum { NET_SUPPORTED, NET_FULLSCREEN, NET_WM_STATE, NET_ACTIVE, NET_WM_NAME, NET_COUNT };

/**
 * Dynamic argument thing
 * com - command to run [spawn()]
 * i   - state indication
 * f   - some float [resize_master()]
 * v   - anything else
**/
typedef union {
    const char** com;
    const int i;
    const void *v;
    const float f;
} Arg;

/**
 * Key (combination)
 * mod     - modifier key mask
 * keysym  - actual key
 * func    - triggered function
 * arg     - function argument
**/
typedef struct {
    unsigned int mod;
    KeySym keysym;
    void (*func)(const Arg *);
    const Arg arg;
} Key;

/**
 * rule for placing windows on desktops
 * read: place class on #[desktop]
**/
typedef struct {
    const char *class;
    const int desktop;
} AppRule;

/**
 * defines a window with its properties
 * next    - next client on stack (NULL if last)
 * isurgn  - set if win is urgent
 * isfull  - set if win is fullscreen
 * isfloat - set if win should float (e.g. tile() ignore flag)
 * istrans - set if win is transient
 * win     - client window
**/
typedef struct Client {
    struct Client *next;
    Bool isurgn, isfull, isfloat, istrans;
    Window win;
} Client;

/**
 * defines a desktop with its properties
 * masz - master area size
 * sasz - first window on stack size
 * sbar - whether to show the bar
 * head - first master window
 * prev - previously focused window
 * curr - currently focused window
**/
typedef struct {
    int masz, sasz, sbar;
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
static void resize_master(const Arg *arg);
static void spawn(const Arg *arg);
static void swap_master();
static void toggle_fullscreen();
static void toggle_bar();

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

static Bool deskhasurgn(Desktop *d);
static void printbar();

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

/**
 * global vars
 * running     - is the wm running and processing events?
 * wh, ww      - screen dimensions
 * currdeskidx - index of the current desktop
 * prevdeskidk - index of the previously focused desktop
 * retval      - value to return at end
 * dis         - X display
 * root        - root window
 * wm|netatoms - wm/netatoms that are handled (ICCCM/EWMH)
 * utf8_atom_t - type for utf8 string atoms
 * desktops    - array of handled desktops
 * master_mod  - global master modifier
**/
static Bool running = True;
static int wh, ww, currdeskidx, prevdeskidx, retval = 0;
static unsigned int numlockmask, win_unfocus, win_focus, win_focus_urgn, win_unfocus_urgn, bgcol, fgcol;
static Display *dis;
static Window root;
static Atom wmatoms[WM_COUNT], netatoms[NET_COUNT], utf8_atom_type;
static Desktop desktops[DESKNUM];
static float master_mod = 0.0f;

/**
 * event handler mapping
 * [EventName] = handler_function
**/
static void (*events[LASTEvent])(XEvent *e) = {
    [KeyPress]         = keypress,         [EnterNotify]    = enternotify,
    [MapRequest]       = maprequest,       [DestroyNotify]  = destroynotify,
    [UnmapNotify]      = unmapnotify,      [PropertyNotify] = propertynotify,
    [ConfigureRequest] = configurerequest, [FocusIn]        = focusin,
};

/**
 * create Client for w and
 * add as master of d
**/
Client* addwindow(Window w, Desktop *d) {
    Client *c = NULL;
    if (!(c = (Client *)calloc(1, sizeof(Client))))
        err(EXIT_FAILURE, "cannot allocate client");
    if (!d->head)
        d->head = c;
#ifdef ADD_AT_BOTTOM
    /* prevclient(d->head, d) returns last window */
    else if (d->head->next)
        prevclient(d->head, d)->next = c;
    else
        d->head->next = c;
#else
    else {
        c->next = d->head;
        d->head = c;
    }
#endif

    XSelectInput(dis, (c->win = w), PropertyChangeMask|FocusChangeMask|(FOLLOW_MOUSE?EnterWindowMask:0));
    printbar();
    return c;
}

/**
 * change desktop to index arg->i
 * 
 * first maps then unmaps to patch flickers
**/
void change_desktop(const Arg *arg) {
    /* sanity checks */
    if (arg->i == currdeskidx || arg->i < 0 || arg->i >= DESKNUM)
        return;
    /* d: prev, n: to_switch_to */
    Desktop *prevd = &desktops[(prevdeskidx = currdeskidx)], *nextd = &desktops[(currdeskidx = arg->i)];
    /* map windows, one by one */
    if (nextd->curr)
        XMapWindow(dis, nextd->curr->win);
    for (Client *c = nextd->head; c; c = c->next)
        XMapWindow(dis, c->win);
    XChangeWindowAttributes(dis, root, CWEventMask, &(XSetWindowAttributes){.do_not_propagate_mask = SubstructureNotifyMask});
    /* unmap old windows, one by one */
    for (Client *c = prevd->head; c; c = c->next) {
        if (c != prevd->curr)
            XUnmapWindow(dis, c->win);
    }
    if (prevd->curr)
        XUnmapWindow(dis, prevd->curr->win);
    XChangeWindowAttributes(dis, root, CWEventMask, &(XSetWindowAttributes){.event_mask = ROOTMASK});
    /* tile and focus */
    if (n->head) {
        tile(n);
        focus(n->curr, n);
    }
    printbar();
}

/**
 * cleans up everyhting
**/
void cleanup(void) {
    Window root_return, parent_return, *children;
    unsigned int nchildren;

    XUngrabKey(dis, AnyKey, AnyModifier, root);
    XQueryTree(dis, root, &root_return, &parent_return, &children, &nchildren);
    /* kill windows */
    for (unsigned int i = 0; i < nchildren; i++)
        deletewindow(children[i]);
    if (children)
        XFree(children);
    XSync(dis, False);
}

/**
 * move current client to desktop #[arg->i]
 * push to stack (e.g. last client)
**/
void client_to_desktop(const Arg *arg) {
    /* sanity checks */
    if (arg->i == currdeskidx || arg->i < 0 || arg->i >= DESKNUM || !desktops[currdeskidx].curr)
        return;
    Desktop *prevd = &desktops[currdeskidx], *destd = &desktops[arg->i];
    Client *c = prevd->curr, *p = prevclient(prevd->curr, prevd), *l = prevclient(destd->head, destd);

    /* remove client from prevd */
    if (prevd->head == c || !p)
        prevd->head = c->next;
    else
        p->next = c->next;
    c->next = NULL;
    XChangeWindowAttributes(dis, root, CWEventMask, &(XSetWindowAttributes){.do_not_propagate_mask = SubstructureNotifyMask});
    /* unmap window */
    if (XUnmapWindow(dis, c->win))
        focus(prevd->prev, prevd);
    XChangeWindowAttributes(dis, root, CWEventMask, &(XSetWindowAttributes){.event_mask = ROOTMASK});
    /* re-tile prevd */
    if (prevd->head && !prevd->head->next)
        tile(prevd);

    /* insert c after l into destd, focus */
    focus(l ? (l->next = c):destd->head ? (destd->head->next = c):(destd->head = c), destd);

    change_desktop(arg);
    printbar();
}

/**
 * handles configure request
 * 
 * some windows set some preferred x, y, w, h, border width, ...
 * stuff, this function reads them and sets them for the window.
 * this is because some applications crash if they don't get what
 * they asked for, so we pretend to respect their wishes and
 * set their preferred stuff only to BETRAY THEM DIRECTLY AFTER
 * AND FORCE OUR TILING IDEALS ONTO THEIR GEOMETRY MUHAHAHAAA
**/
void configurerequest(XEvent *e) {
    XConfigureRequestEvent *ev = &e->xconfigurerequest;
    XWindowChanges wc = { ev->x, ev->y, ev->width, ev->height, ev->border_width, ev->above, ev->detail };
    if (XConfigureWindow(dis, ev->window, ev->value_mask, &wc))
        XSync(dis, False);
    Desktop *d = NULL; Client *c = NULL;
    if (wintoclient(ev->window, &c, &d))
        tile(d);
    printbar();
}

/**
 * send WM_DELETE_WINDOW message to w
 * this should close the client
 * (e.g. [x] btn on regular wm)
**/
void deletewindow(Window w) {
    XEvent ev = { .type = ClientMessage };
    ev.xclient.window = w;
    ev.xclient.format = 32;
    ev.xclient.message_type = wmatoms[WM_PROTOCOLS];
    ev.xclient.data.l[0]    = wmatoms[WM_DELETE_WINDOW];
    ev.xclient.data.l[1]    = CurrentTime;
    XSendEvent(dis, w, False, NoEventMask, &ev);
    printbar();
}

/**
 * executed whenever a window is destroyed (closed/...)
 * it removes the client
**/
void destroynotify(XEvent *e) {
    Desktop *d = NULL; Client *c = NULL;
    if (wintoclient(e->xdestroywindow.window, &c, &d))
        removeclient(c, d);
    printbar();
}

/**
 * when the mouse pointer enters a Client's region,
 * focus that client
 * no check for FOLLOW_MOUSE because if it is false,
 * no handler will have been added for it in the first place
**/
void enternotify(XEvent *e) {
    Desktop *d = NULL; Client *c = NULL, *p = NULL;

    if ((e->xcrossing.mode != NotifyNormal && e->xcrossing.detail == NotifyInferior)
        || !wintoclient(e->xcrossing.window, &c, &d) || e->xcrossing.window == d->curr->win) return;

    if ((p = d->prev))
        XChangeWindowAttributes(dis, p->win, CWEventMask, &(XSetWindowAttributes){.do_not_propagate_mask = EnterWindowMask});
    focus(c, d);
    if (p) XChangeWindowAttributes(dis, p->win, CWEventMask, &(XSetWindowAttributes){.event_mask = EnterWindowMask});
    printbar();
}

/**
 * sets curr and prev for d, restacks clients,
 * manages borders and then focuses curr
**/
void focus(Client *c, Desktop *d) {
    /**
     * if there is no client on d or c is NULL,
     * set curr and prev to NULL and return
     * if c was previously focused and curr was destroyed,
     * meaning focus(prev) was called, then set curr to c
     * and prev to prevclient(curr, d)
     * otherwise, prev_win() was called, then set
     * prev to curr and curr to c
    **/
    if (!d->head || !c) {
        d->curr = d->prev = NULL;
        return;
    } else if (d->prev == c && d->curr != c->next) {
        d->curr = c;
        d->prev = prevclient(d->curr, d);
    } else if (d->curr != c) {
        d->prev = d->curr;
        d->curr = c;
    }

    /**
     * restack clients
     *
     * order is as follows:
     *  - current when floating or transient
     *  - floating || transient windows
     *  - current when tiled
     *  - current when fullscreen
     *  - fullscreen windows
     *  - tiled windows
     *
     * num of n:all fl:fullscreen ft:floating/transient windows
    **/
    int n = 0, fl = 0, ft = 0;
    for (c = d->head; c; c = c->next, ++n) {
        if (ISFFT(c)) {
            fl++;
            if (!c->isfull)
                ft++;
        }
    }
    Window w[n];
    w[ft] = d->curr->win;
    for (fl += !ISFFT(d->curr) ? 1:0, c = d->head; c; c = c->next) {
        XSetWindowBorder(dis, c->win, c == d->curr ? win_focus:win_unfocus);
        XSetWindowBorderWidth(dis, c->win, c->isfull || (!ISFFT(c) &&
            (d->head->isfull || !d->head->next)) ? 0:BORDER_SIZE);
        if (c != d->curr)
            w[c->isfull ? --fl:ISFFT(c) ? --ft:--n] = c->win;
    }
    XRestackWindows(dis, w, LENGTH(w));

    XSetInputFocus(dis, d->curr->win, RevertToPointerRoot, CurrentTime);
    XSync(dis, False);
    printbar();
}

/**
 * some windows steal focus, this
 * function sets the focus back to the
 * window focused by the user/wm
**/
void focusin(XEvent *e) {
    Desktop *d = &desktops[currdeskidx];
    if (d->curr && d->curr->win != e->xfocus.window)
        focus(d->curr, d);
}

/**
 * register KeyEvent and listen for it
**/
void grabkeys(void) {
    KeyCode code;
    XUngrabKey(dis, AnyKey, AnyModifier, root);
    unsigned int k, m, modifiers[] = { 0, LockMask, numlockmask, numlockmask|LockMask };

    for (k = 0, m = 0; k < LENGTH(keys); k++, m = 0) {
        while ((code = XKeysymToKeycode(dis, keys[k].keysym)) && m < LENGTH(modifiers))
            XGrabKey(dis, code, keys[k].mod|modifiers[m++], root, True, GrabModeAsync, GrabModeAsync);
    }
}

/**
 * call handler for KeyEvents registered
 * by grabkeys(void)
**/
void keypress(XEvent *e) {
    KeySym keysym = XkbKeycodeToKeysym(dis, e->xkey.keycode, 0, 0);
    for (unsigned int i = 0; i < LENGTH(keys); i++) {
        if (keysym == keys[i].keysym && CLEANMASK(keys[i].mod) == CLEANMASK(e->xkey.state)) {
            if (keys[i].func)
                keys[i].func(&keys[i].arg);
        }
    }
}

/**
 * close a win using WM_DELETE_WINDOW request
 * if this is not supported, just kill the
 * client and go on with life
**/
void close_win(void) {
    Desktop *d = &desktops[currdeskidx];
    /* sanity check */
    if (!d->curr)
        return;

    Atom *prot = NULL; int n = -1;
    /* check for WM_DELETE_WINDOW support */
    if (XGetWMProtocols(dis, d->curr->win, &prot, &n))
        while(--n >= 0 && prot[n] != wmatoms[WM_DELETE_WINDOW]);
    if (n < 0) {
        /* no support? kill and remove client */
        XKillClient(dis, d->curr->win);
        removeclient(d->curr, d);
    } else /* support? nicely close (will NOT work for frozen programs) */
        deletewindow(d->curr->win);
    if (prot)
        XFree(prot);
    printbar();
}

/**
 * called when a window is like "hey user look at me you fuckhead!"
 * if the window is already shown that's up to the user
 * if override_redirect is set we don't care about what the window wants
 * 
 * move the window to the desktop where is should be (AppRuleExists ? AppRuleDeskIdx : currdeskidx)
 * focus the desktop where the window went
**/
void maprequest(XEvent *e) {
    Desktop *d = NULL; Client *c = NULL;
    Window w = e->xmaprequest.window;
    XWindowAttributes wa = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    if (wintoclient(w, &c, &d) || (XGetWindowAttributes(dis, w, &wa) && wa.override_redirect)) return;

    XClassHint ch = {0, 0};
    Bool follow = False, floating = False;
    int newdsk = currdeskidx;

    if (XGetClassHint(dis, w, &ch)) for (unsigned int i = 0; i < LENGTH(rules); i++)
        if (strstr(ch.res_class, rules[i].class) || strstr(ch.res_name, rules[i].class)) {
            if (rules[i].desktop >= 0 && rules[i].desktop < DESKNUM)
                newdsk = rules[i].desktop;
            floating = rules[i].floating;
            follow = True;
            break;
        }
    if (ch.res_class) XFree(ch.res_class);
    if (ch.res_name) XFree(ch.res_name);

    c = addwindow(w, (d = &desktops[newdsk]));
    c->istrans = XGetTransientForHint(dis, c->win, &w);
    c->isfloat = floating;
    /* we devilishly ignore fullscreen requests!
       someone, please document this later! */

    if (currdeskidx == newdsk) {
        if (!ISFFT(c))
            tile(d);
        XMapWindow(dis, c->win);
    } else if (follow)
        change_desktop(&(Arg){.i = newdsk});
    focus(c, d);
}

/**
 * swap curr and prev of curr
**/
void move_up(void) {
    Desktop *d = &desktops[currdeskidx];
    if (!d->curr || !d->head->next)
        return;

    /* p is prev from curr, pp is prev from p */
    Client *pp = NULL, *p = prevclient(d->curr, d);
    if (p->next)
        for (pp = d->head; pp && pp->next != p; pp = pp->next);

    if (pp)
        pp->next = d->curr;
    else
        d->head = (d->curr == d->head) ? d->curr->next:d->curr;

    p->next = (d->curr->next == d->head) ? d->curr:d->curr->next;
    d->curr->next = (d->curr->next == d->head) ? NULL:p;
    tile(d);

    printbar();
}

/**
 * swap curr and next
**/
void move_down(void) {
    Desktop *d = &desktops[currdeskidx];
    if (!d->curr || !d->head->next)
        return;

    Client *p = prevclient(d->curr, d), *n = (d->curr->next) ? d->curr->next:d->head;

    if (d->curr == d->head)
        d->head = n;
    else
        p->next = d->curr->next;
    d->curr->next = (d->curr->next) ? n->next:n;

    if (d->curr->next == n->next)
        n->next = d->curr;
    else
        d->head = d->curr;
    tile(d);

    printbar();
}

/**
 * focus next window
 * if there is no next, focus head
**/
void next_win(void) {
    Desktop *d = &desktops[currdeskidx];
    if (d->curr && d->head->next)
        focus(d->curr->next ? d->curr->next:d->head, d);
    printbar();
}

/**
 * focus previous window
 * if there is no prev, focus stack tail
**/
void prev_win(void) {
    Desktop *d = &desktops[currdeskidx];
    if (d->curr && d->head->next)
        focus(prevclient(d->curr, d), d);
    printbar();
}

/**
 * set urgent hint of window
 * if urgent, set isurgn and recolor border
**/
void propertynotify(XEvent *e) {
    Desktop *d = NULL; Client *c = NULL;
    if (e->xproperty.atom != XA_WM_HINTS || !wintoclient(e->xproperty.window, &c, &d)) return;

    /* get isurgn */
    XWMHints *wmh = XGetWMHints(dis, c->win);
    c->isurgn = (c != desktops[currdeskidx].curr && wmh && (wmh->flags & XUrgencyHint));
    if (c->isurgn) {
        XSetWindowBorder(dis, c->win, c == d->curr ? win_focus_urgn:win_unfocus_urgn);
    }

    if (wmh)
        XFree(wmh);
    printbar();
}

/**
 * quit badwm (with EXIT_SUCCESS)
**/
void quit() {
    retval = 0;
    running = False;
}

/**
 * remove a client from a desktop,
 * then retile 'n stuff
**/
void removeclient(Client *c, Desktop *d) {
    Client **p = NULL;
    /* check if c is on d */
    for (p = &d->head; *p && (*p != c); p = &(*p)->next);
    if (!*p)
        return;
    else
        *p = c->next;
    /* p is c's successor from now on */
    /* re-set d->prev if necessary */
    if (c == d->prev && !(d->prev = prevclient(d->curr, d)))
        d->prev = d->head;
    /* if c is focused, focus d->prev */
    if (c == d->curr || (d->head && !d->head->next))
        focus(d->prev, d);
    if (d->head)
        tile(d);
    free(c);
    printbar();
}

/**
 * set fullscreen state of a client
**/
void setfullscreen(Client *c, Desktop *d, Bool fullscrn) {
    c->isfull = fullscrn;
    if (fullscrn)
        XMoveResizeWindow(dis, c->win, 0, 0, ww, wh + PANEL_HEIGHT);
    else
        tile(d);
    XSetWindowBorderWidth(dis, c->win, (c->isfull || !d->head->next ? 0:BORDER_SIZE));
    printbar();
}

/**
 * toggle fullscreen state of a client
**/
void toggle_fullscreen() {
    Desktop *d = &desktops[currdeskidx];
    Client *c = d->head;
    /* sanity check */
    if (!d->head)
        return;
    setfullscreen(c, d, !c->isfull);
}

/**
 * toggle visibility of the bar
 */
void toggle_bar() {
    desktops[currdeskidx].sbar = !desktops[currdeskidx].sbar;
    printbar();
    tile(&desktops[currdeskidx]);
}

/**
 * init the window manager,
 * set & get init values, ...
**/
void initwm(void) {
    sigchld(0);

    /* get screen & root win */
    const int screen = DefaultScreen(dis);
    root = RootWindow(dis, screen);

    /* init vars */
    ww = XDisplayWidth(dis, screen);
    wh = XDisplayHeight(dis, screen) - PANEL_HEIGHT;

    for (unsigned int d = 0; d < DESKNUM; d++)
        desktops[d] = (Desktop){ .sbar = 1 };

    /* get colors */
    win_focus = getcolor(FOCUS, screen);
    win_unfocus = getcolor(UNFOCUS, screen);
    win_focus_urgn = getcolor(URGENTFOCUS, screen);
    win_unfocus_urgn = getcolor(URGENTUNFOCUS, screen);
    bgcol = getcolor(BACKGROUNDCOL, screen);
    fgcol = getcolor(FOREGROUNDCOL, screen);

    /* get mod keys */
    XModifierKeymap *modmap = XGetModifierMapping(dis);
    for (int k = 0; k < 8; k++) for (int j = 0; j < modmap->max_keypermod; j++)
        if (modmap->modifiermap[modmap->max_keypermod*k + j] == XKeysymToKeycode(dis, XK_Num_Lock))
            numlockmask = (1 << k);
    XFreeModifiermap(modmap);

    /* init wm atoms */
    wmatoms[WM_PROTOCOLS]      = XInternAtom(dis, "WM_PROTOCOLS",     False);
    wmatoms[WM_DELETE_WINDOW]  = XInternAtom(dis, "WM_DELETE_WINDOW", False);
    netatoms[NET_SUPPORTED]    = XInternAtom(dis, "_NET_SUPPORTED",   False);
    netatoms[NET_WM_STATE]     = XInternAtom(dis, "_NET_WM_STATE",    False);
    netatoms[NET_WM_NAME]      = XInternAtom(dis, "_NET_WM_NAME",     False);
    netatoms[NET_ACTIVE]       = XInternAtom(dis, "_NET_ACTIVE_WINDOW",       False);
    netatoms[NET_FULLSCREEN]   = XInternAtom(dis, "_NET_WM_STATE_FULLSCREEN", False);

    utf8_atom_type    = XInternAtom(dis, "UTF8_STRING", False);

    /* set _NET_WM_NAME (for pfetch, neofetch, ufetch, etc...) */
    XChangeProperty(dis, root, netatoms[NET_WM_NAME], utf8_atom_type, 8, PropModeReplace, (unsigned char *)"badwm", 6);
    /* tell the X Server that we support all the netatoms we have in netatoms[] */
    XChangeProperty(dis, root, netatoms[NET_SUPPORTED], XA_ATOM, 32, PropModeReplace, (unsigned char *)netatoms, NET_COUNT);

    /* init error handler & input */
    XSetErrorHandler(xerrorstart);
    XSelectInput(dis, root, ROOTMASK);
    XSync(dis, False);
    XSetErrorHandler(xerror);
    XSync(dis, False);

    /* init key handlers & change to first desktop */
    grabkeys();
    change_desktop(&(Arg){.i = 0});
}

/**
 * process events
**/
void runwm(void) {
    XEvent ev;
    while(running && !XNextEvent(dis, &ev)) {
        if (events[ev.type])
            events[ev.type](&ev);
    }
}

/**
 * swap current client with master
**/
void swap_master(void) {
    Desktop *d = &desktops[currdeskidx];
    if (!d->curr || !d->head->next)
        return;
    if (d->curr == d->head)
        move_down();
    else
        while (d->curr != d->head) {
            move_up();
        }
    focus(d->head, d);
}

/**
 * convert hex color to X11 color
**/
unsigned long getcolor(const char* color, const int screen) {
    XColor c; Colormap map = DefaultColormap(dis, screen);
    if (!XAllocNamedColor(dis, map, color, &c, &c))
        err(EXIT_FAILURE, "cannot allocate color");
    return c.pixel;
}

/**
 * SIGCHLD handler
**/
void sigchld(__attribute__((unused)) int sig) {
    if (signal(SIGCHLD, sigchld) != SIG_ERR)
        while(0 < waitpid(-1, NULL, WNOHANG));
    else
        err(EXIT_FAILURE, "cannot install SIGCHLD handler");
}

/**
 * gets prev of c on d
**/
Client* prevclient(Client *c, Desktop *d) {
    Client *p = NULL;
    if (c && d->head && d->head->next) {
        for (p = d->head; p->next && p->next != c; p = p->next);
    }
    return p;
}

/**
 * change current desktop-local master area
**/
void resize_master(const Arg *arg) {
    Desktop *d = &desktops[currdeskidx];
    if (MASTER_SIZE + (d->masz + arg->f) > 0.0f &&
        MASTER_SIZE + (d->masz + arg->f) < 1.0f) {
        d->masz += arg->f;
    }
    tile(&desktops[currdeskidx]);
}

/**
 * run arg->com detached
**/
void spawn(const Arg *arg) {
    if (fork()) return;
    if (dis) close(ConnectionNumber(dis));
    setsid();
    execvp((char*)arg->com[0], (char**)arg->com);
    err(EXIT_SUCCESS, "execvp %s", (char *)arg->com[0]);
    printbar();
}

/**
 * call appropriate tiling function
**/
void tile(Desktop *d) {
    if (!d->head) return;
    if (d->head->next && !d->head->isfull) {
        stack(0, TOP_PANEL && d->sbar ? PANEL_HEIGHT:0,
              ww, wh + (d->sbar ? 0:PANEL_HEIGHT), d);
    } else {
        fullscreen(0, TOP_PANEL && d->sbar ? PANEL_HEIGHT:0,
                   ww, wh + (d->sbar ? 0:PANEL_HEIGHT), d);
    }
    printbar();
}

/**
 * layout handlers
 * 
 * params:
 * x - start x offset
 * y - start y offset
 * w - width
 * h - height
 * d - to-be-tiled desktop
 */

/**
 * fullscreen tiling handler
**/
void fullscreen(int x, int y, int w, int h, const Desktop *d) {
    for (Client *c = d->head; c; c = c->next) if (!ISFFT(c)) XMoveResizeWindow(dis, c->win, x, y, w, h);
}

/**
 * stack tiling handler
**/
void stack(int x, int y, int w, int h, const Desktop *d) {
    Client *c = NULL, *t = NULL; Bool b = BSTACK;
    int n = 0, p = 0, z = (b ? w:h), ma = (b ? h:w) * (MASTER_SIZE + d->masz);

    for (t = d->head; t; t = t->next) {
        if (!ISFFT(t)) {
            if (c)
                ++n;
            else
                c = t;
        }
    }

    if (c && !n) XMoveResizeWindow(dis, c->win, x + GAPS, y + GAPS,
                                   w - 2*(BORDER_SIZE + GAPS), h - 2*(BORDER_SIZE + GAPS));
    if (!c || !n) {
        return;
    } else if (n > 1) {
        p = (z - d->sasz)%n + d->sasz;
        z = (z - d->sasz)/n;
    }

    if (b) {
        XMoveResizeWindow(dis, c->win, x + GAPS, y + GAPS,
                          w - 2*(BORDER_SIZE + GAPS), ma - 2*(BORDER_SIZE + GAPS));
    } else {
        XMoveResizeWindow(dis, c->win, x + GAPS, y + GAPS,
                          ma - 2*(BORDER_SIZE + GAPS), h - 2*(BORDER_SIZE + GAPS));
    }

    for (c = c->next; c && ISFFT(c); c = c->next);
    int ch = z - 2*BORDER_SIZE - GAPS, cw = (b ? h:w) - 2*BORDER_SIZE - ma - GAPS;

    if (b)
        XMoveResizeWindow(dis, c->win, x += GAPS, y += ma, ch - GAPS + p, cw);
    else
        XMoveResizeWindow(dis, c->win, x += ma, y += GAPS, cw, ch - GAPS + p);

    for (b ? (x += z+p-GAPS) : (y += z+p-GAPS), c = c->next; c; c = c->next) {
        if (ISFFT(c)) continue;
        if (b) {
            XMoveResizeWindow(dis, c->win, x, y, ch, cw);
            x += z;
        } else {
            XMoveResizeWindow(dis, c->win, x, y, cw, ch);
            y += z;
        }
    }
}

/**
 * delete clients of invisible windows so
 * that we do not draw void because that
 * might cause a portal to the X11 underworld
 * to be opened
**/
void unmapnotify(XEvent *e) {
    Desktop *d = NULL; Client *c = NULL;
    if (wintoclient(e->xunmap.window, &c, &d)) removeclient(c, d);
}

/**
 * find client and desktop that w is on
**/
Bool wintoclient(Window w, Client **c, Desktop **d) {
    for (int i = 0; i < DESKNUM && !*c; i++)
        for (*d = &desktops[i], *c = (*d)->head; *c && (*c)->win != w; *c = (*c)->next);
    return (*c != NULL);
}

/**
 * checks if d has an urgent window
**/
Bool deskhasurgn(Desktop *d) {
    Bool urgent = False;
    for (Client *c = d->head; c; urgent |= c->isurgn, c = c->next);
    return urgent;
}

/**
 * print bar info
**/
void printbar() {
    /**
     * $DESKNUM_TOTAL:$DESKFOCUS $($HASWIN:$HASURGN:$SHOWBAR)[for desk in desktops]
    **/

    printf("%d:%d", DESKNUM, currdeskidx);

    for (int i = 0; i < DESKNUM; i++) {
        printf(" %d:%d:%d", desktops[i].head != NULL, deskhasurgn(&desktops[i]), desktops[i].sbar);
    }

    putchar('\n');
    fflush(stdout);
}

/**
 * X error handler
**/
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

/**
 * we can't start if there's already someone managing the windows
**/
int xerrorstart(__attribute__((unused)) Display *dis, __attribute__((unused)) XErrorEvent *ee) {
    errx(EXIT_FAILURE, "CRITICAL: Another window manager is already running!");
}

int main(int argc, char const *argv[]) {
    if (argc == 2 && !strncmp(argv[1], "-v", 3))
        errx(EXIT_SUCCESS, "badwm - The small, fast window manager");
    if(!(dis = XOpenDisplay(NULL))) errx(EXIT_FAILURE, "CRITICAL: Could not open X Display - terminating!");

    initwm();
    runwm();

    cleanup();
    XCloseDisplay(dis);
    return retval;
}
