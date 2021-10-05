#ifndef CONFIG_H
#define CONFIG_H

/* Config goes here */

// Gaps between windows in pixels. Can be 0 for no gaps
#define GAPS 8

// This defines the mod key.
// Every shortcut may only define for example #define RAISE_WINDOW T
// but the shortcut is Mod+T. This defaults to the left super/windows key
#define MOD_K           Mod1Mask    /* mod key */
#define CONTROL         ControlMask /* Control key */
#define SHIFT           ShiftMask   /* Shift key */

/** generic settings **/
#define MASTER_SIZE     0.55       /* percent (:100) the master window takes up of the screen space */
#define DESKNUM         4

#define TERMINAL        "urxvt"

#define TOP_PANEL       False     /* False means panel is on bottom */
#define PANEL_HEIGHT    24        /* 0 for no space for panel, thus no panel */

#define FOLLOW_MOUSE    True      /* focus the window the mouse just entered */
#define BSTACK          False     /* set to true for bottom stack */

#define BORDER_SIZE     2         /* window border size in pixels */
#define FOCUS           "#ebdbb2" /* focused window border color  another alternative: 0e8fff*/
#define UNFOCUS         "#3c3836" /* unfocused window border color  */
#define URGENTFOCUS     "#fb4934" /* focused urgent window border bg color */
#define URGENTUNFOCUS   "#cc241d" /* unfocused urgent window border bg color */
#define BACKGROUNDCOL   "#282828" /* used for background */
#define FOREGROUNDCOL   "#83a598" /* used for all the text */
#define PANELSTARTOFST  6         /* space between top left corner and text start */

/**
 * open applications to specified desktop with specified mode.
 * if desktop is negative, then current is assumed
 */
static const AppRule rules[] = { \
    /*  class     desktop */
    { "Firefox",     2 },
};

/* helper for spawning shell commands */
#define SHCMD(cmd) {.com = (const char*[]){"/bin/sh", "-c", cmd, NULL}}

/**
 * custom commands
 * must always end with ', NULL };'
 */
static const char *termcmd[] = { TERMINAL,                                                                             NULL };
static const char *menucmd[] = { "dmenu_run", "-nb", "#282828", "-nf", "#928374", "-sb", "#3c3836", "-sf", "#a89984",  NULL };
static const char *filecmd[] = { TERMINAL, "-e", "ranger",                                                             NULL };
static const char *htopcmd[] = { TERMINAL, "-e", "htop",                                                               NULL };

#define DESKTOPCHANGE(K,N) \
    {  MOD_K,             K,              change_desktop, {.i = N}}, \
    {  MOD_K|ShiftMask,   K,              client_to_desktop, {.i = N}},

/**
 * keyboard shortcuts
 */
static Key keys[] = {
    /* modifier           key            function           argument */
    {  MOD_K|SHIFT,       XK_q,          close_win,         {NULL}},
    {  MOD_K,             XK_j,          next_win,          {NULL}},
    {  MOD_K,             XK_k,          prev_win,          {NULL}},
    {  MOD_K|SHIFT,       XK_Return,     swap_master,       {NULL}},
    {  MOD_K|SHIFT,       XK_j,          move_down,         {NULL}},
    {  MOD_K|SHIFT,       XK_k,          move_up,           {NULL}},
    {  MOD_K,             XK_f,          toggle_fullscreen, {NULL}},
    {  MOD_K|CONTROL,     XK_e,          quit,              {NULL}},
    {  MOD_K,             XK_Return,     spawn,             {.com = termcmd}},
    {  MOD_K,             XK_d,          spawn,             {.com = menucmd}},
    {  MOD_K,             XK_r,          spawn,             {.com = filecmd}},
/*    {  MOD_K,             XK_m,          spawn,             {.com = mtrxcmd}}, */
    {  MOD_K,             XK_h,          spawn,             {.com = htopcmd}},
    {  MOD_K,             XK_a,          resize_master,     {.f = -0.05f}},
    {  MOD_K,             XK_s,          resize_master,     {.f =  0.05f}},
    {  MOD_K,             XK_p,          toggle_bar,        {NULL}},
       DESKTOPCHANGE(     XK_F1,                            0)
       DESKTOPCHANGE(     XK_F2,                            1)
       DESKTOPCHANGE(     XK_F3,                            2)
       DESKTOPCHANGE(     XK_F4,                            3)
};

#endif // CONFIG_H
