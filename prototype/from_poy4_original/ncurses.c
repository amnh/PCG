/* POY 4.0 Beta. A phylogenetic analysis program using Dynamic Homologies.    */
/* Copyright (C) 2007  Andrés Varón, Le Sy Vinh, Illya Bomash, Ward Wheeler,  */
/* and the American Museum of Natural History.                                */
/*                                                                            */
/* This program is free software; you can redistribute it and/or modify       */
/* it under the terms of the GNU General Public License as published by       */
/* the Free Software Foundation; either version 2 of the License, or          */
/* (at your option) any later version.                                        */
/*                                                                            */
/* This program is distributed in the hope that it will be useful,            */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              */
/* GNU General Public License for more details.                               */
/*                                                                            */
/* You should have received a copy of the GNU General Public License          */
/* along with this program; if not, write to the Free Software                */
/* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301   */
/* USA                                                                        */

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/bigarray.h"
#include "caml/fail.h"
#include "caml/custom.h"
#include "caml/intext.h"
#include "caml/alloc.h"
#include "caml/callback.h"
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "config.h"
#ifdef HAVE_LIBNCURSES
#ifndef _WIN32
#include <curses.h>
#include <sys/ioctl.h>
#include <menu.h>
#else
#include <curses.h>
#endif

/* First, Windows! */

#define Win_custom(v) *((WINDOW **) Data_custom_val(v))

void
ncurs_CAML_window_free (value m) {
    return;
}

static struct custom_operations windows = {
    "http://www.amnh.org/poy/ncurses/windows0.1",
    &ncurs_CAML_window_free,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

value
ncurs_CAML_scroll (value vw, value vs) {
    CAMLparam2(vw, vs);
    wscrl(Win_custom(vw), Int_val(vs));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_scrollok (value vw, value vb) {
    CAMLparam2(vw, vb);
    scrollok(Win_custom(vw), Bool_val(vb));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_idlok (value vw, value vb) {
    CAMLparam2(vw, vb);
    idlok(Win_custom(vw), Bool_val(vb));
    CAMLreturn(Val_unit);
}


value
ncurs_CAML_create (value vp, value vh, value vw, value vy, value vx) {
    CAMLparam5(vp, vh, vw, vy, vx);
    // CAMLlocal1(res);
    WINDOW **tmp;
    res = alloc_custom (&windows, sizeof (WINDOW *), 1, 1000000);
    tmp = (WINDOW **) Data_custom_val(res);
    *tmp = derwin (Win_custom(vp), Int_val(vh), Int_val(vw), Int_val(vy), Int_val(vx));
    CAMLreturn(res);
}

value
ncurs_CAML_delete (value vp) {
    CAMLparam1(vp);
    delwin(Win_custom(vp));
    CAMLreturn(Val_unit);
}

/* Getting windows size */
value
ncurs_CAML_getyx (value vwin) {
    CAMLparam1(vwin);
    // CAMLlocal1(res);
    int y, x;
    getyx(Win_custom(vwin), y, x);
    res = caml_alloc_tuple(2);
    Store_field(res, 0, Val_int(y));
    Store_field(res, 1, Val_int(x));
    CAMLreturn(res);
}

value
ncurs_CAML_getparyx (value vwin) {
    CAMLparam1(vwin);
    // CAMLlocal1(res);
    int x, y;
    getparyx(Win_custom(vwin), y, x);
    res = caml_alloc_tuple(2);
    Store_field(res, 0, Val_int(y));
    Store_field(res, 1, Val_int(x));
    CAMLreturn(res);
}

value
ncurs_CAML_getbegyx (value vwin) {
    CAMLparam1(vwin);
    // CAMLlocal1(res);
    int x, y;
    getbegyx(Win_custom(vwin), y, x);
    res = caml_alloc_tuple(2);
    Store_field(res, 0, Val_int(y));
    Store_field(res, 1, Val_int(x));
    CAMLreturn(res);
}

value
ncurs_CAML_getmaxyx (value vwin) {
    CAMLparam1(vwin);
    // CAMLlocal1(res);
    int x, y;
    getmaxyx(Win_custom(vwin), y, x);
    res = caml_alloc_tuple(2);
    Store_field(res, 0, Val_int(y));
    Store_field(res, 1, Val_int(x));
    CAMLreturn(res);
}

/** Setting some windows parameters */
value
ncurs_CAML_box (value win, value a, value b) {
    CAMLparam3(win, a, b);
    box(Win_custom(win), (char) Int_val(a), (char) Int_val(b));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_wrefresh (value win) {
    CAMLparam1(win);
    wrefresh (Win_custom(win));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_refresh (value unit) {
    CAMLparam1(unit);
    refresh ();
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_wnoutrefresh (value win) {
    CAMLparam1(win);
    wnoutrefresh(Win_custom(win));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_doupdate (value unit) {
    CAMLparam1(unit);
    doupdate ();
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_echo (value tf) {
    CAMLparam1(tf);
    if (Bool_val(tf)) echo();
    else noecho ();
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_noraw (value v) {
    CAMLparam1(v);
    noraw();
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_raw (value v) {
    CAMLparam1(v);
    raw();
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_nocbreak (value v) {
    CAMLparam1(v);
    nocbreak();
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_cbreak (value v) {
    CAMLparam1(v);
    cbreak();
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_meta (value vwin, value vb) {
    CAMLparam2(vwin, vb);
    meta(Win_custom(vwin), Bool_val(vb));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_keypad (value vwin, value vb) {
    CAMLparam2(vwin, vb);
    keypad(Win_custom(vwin), Bool_val(vb));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_addstr (value vtr) {
    CAMLparam1(vtr);
    char *str;
    str = String_val (vtr);
    addstr (str);
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_waddstr (value vwin, value vstr) {
    CAMLparam2(vwin, vstr);
    char *str;
    str = String_val (vstr);
    waddstr (Win_custom(vwin), str);
    CAMLreturn(Val_unit);
}


value
ncurs_CAML_mvaddstr (value vrow, value vcol, value vstr) {
    CAMLparam3(vrow, vcol, vstr);
    mvaddstr (Int_val(vrow), Int_val(vcol), String_val(vstr));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_mvwaddstr (value vwin, value vrow, value vcol, value vstr) {
    CAMLparam4(vwin, vrow, vcol, vstr);
    mvwaddstr (Win_custom(vwin), Int_val(vrow), Int_val(vcol), \
            String_val(vstr));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_getch (value unit) {
    CAMLparam1(unit);
    int res;
    res = getch();
    CAMLreturn(Val_int (res));
}

/* Attributes */

value
ncurs_CAML_A_STANDOUT (value vwin, value onoff) {
    CAMLparam2(vwin, onoff);
    if (Bool_val(onoff)) wattron(Win_custom(vwin), A_STANDOUT);
    else wattroff(Win_custom(vwin), A_STANDOUT);
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_A_UNDERLINE (value vwin, value onoff) {
    CAMLparam2(vwin, onoff);
    if (Bool_val(onoff)) wattron(Win_custom(vwin), A_UNDERLINE);
    else wattroff(Win_custom(vwin), A_UNDERLINE);
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_A_REVERSE (value vwin, value onoff) {
    CAMLparam2(vwin, onoff);
    if (Bool_val(onoff)) wattron(Win_custom(vwin), A_REVERSE);
    else wattroff(Win_custom(vwin), A_REVERSE);
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_A_BLINK (value vwin, value onoff) {
    CAMLparam2(vwin, onoff);
    if (Bool_val(onoff)) wattron(Win_custom(vwin), A_BLINK);
    else wattroff(Win_custom(vwin), A_BLINK);
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_A_BOLD (value vwin, value onoff) {
    CAMLparam2(vwin, onoff);
    if (Bool_val(onoff)) wattron(Win_custom(vwin), A_BOLD);
    else wattroff(Win_custom(vwin), A_BOLD);
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_A_PROTECT (value vwin, value onoff) {
    CAMLparam2(vwin, onoff);
    if (Bool_val(onoff)) wattron(Win_custom(vwin), A_PROTECT);
    else wattroff(Win_custom(vwin), A_PROTECT);
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_A_INVIS (value vwin, value onoff) {
    CAMLparam2(vwin, onoff);
    if (Bool_val(onoff)) wattron(Win_custom(vwin), A_INVIS);
    else wattroff(Win_custom(vwin), A_INVIS);
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_A_ALTCHARSET (value vwin, value onoff) {
    CAMLparam2(vwin, onoff);
    if (Bool_val(onoff)) wattron(Win_custom(vwin), A_ALTCHARSET);
    else wattroff(Win_custom(vwin), A_ALTCHARSET);
    CAMLreturn(Val_unit);
}

/** Callback to handle window resize */
void resizeHandler (int sig) {
    caml_callback(*caml_named_value("ncursesml_redraw"), Val_unit);
}

/** CAML bindings */
value
ncurs_CAML_init (value unit) {
    CAMLparam1(unit);
    // CAMLlocal1(res);
    WINDOW **tmp;
    initscr();
    res = alloc_custom (&windows, sizeof (WINDOW *), 1, 1000000);
    tmp = (WINDOW **) Data_custom_val(res);
    *tmp = stdscr;
    CAMLreturn (res);
}

value
ncurs_CAML_endwin (value unit) {
    CAMLparam1 (unit);
    endwin ();
    CAMLreturn (Val_unit);
}

value
ncurs_CAML_columns (value unit) {
    CAMLparam1 (unit);
    CAMLreturn (Val_int (COLS));
}

value
ncurs_CAML_lines (value unit) {
    CAMLparam1 (unit);
    CAMLreturn (Val_int (LINES));
}

value
ncurs_CAML_istermresized (value vr, value vc) {
    CAMLparam2(vr, vc);
#ifdef _WIN32
    CAMLreturn(Val_bool(0));
#else
    bool res;
    res = is_term_resized (Int_val(vr), Int_val(vc));
    CAMLreturn(Val_bool(res));
#endif
}

value
ncurs_CAML_wresize (value vw, value vl, value vc) {
    CAMLparam3(vw, vl, vc);
    wresize (Win_custom(vw), Int_val(vl), Int_val(vc));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_mvwin (value vw, value vl, value vc) {
    CAMLparam3(vw, vl, vc);
    mvderwin (Win_custom(vw), Int_val(vl), Int_val(vc));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_wmove (value vw, value vl, value vc) {
    CAMLparam3(vw, vl, vc);
    wmove (Win_custom(vw), Int_val(vl), Int_val(vc));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_wgetch (value vw) {
    CAMLparam1(vw);
    int res;
    res = wgetch (Win_custom(vw));
    CAMLreturn(Val_int(res));
}

value
ncurs_CAML_waddch (value vw, value vc) {
    CAMLparam2(vw, vc);
    waddch (Win_custom(vw), Int_val(vc));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_mvwhline (value vw, value vc, value vl, value vcc, value vn) {
    CAMLparam5(vw, vc, vl, vcc, vn);
    mvwhline (Win_custom(vw), Int_val(vc), Int_val(vl), Int_val(vcc),
            Int_val(vn));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_has_colors (value unit) {
    CAMLparam1(unit);
    bool v;
    v = has_colors();
    CAMLreturn(Val_bool(v));
}

value
ncurs_CAML_start_color (value unit) {
    CAMLparam1(unit);
    start_color();
    CAMLreturn(Val_unit);
}

int color_of_int (int i) {
    switch (i) {
    case 0: return COLOR_RED;
    case 1: return COLOR_GREEN;
    case 2: return COLOR_YELLOW;
    case 3: return COLOR_BLUE;
    case 4: return COLOR_CYAN;
    case 5: return COLOR_MAGENTA;
    case 6: return COLOR_WHITE;
    case 7: return COLOR_BLACK;
    default: return COLOR_WHITE;
    }
}

value
ncurs_CAML_init_pair (value code, value fg, value bg) {
    CAMLparam3(code, fg, bg);
    int ifg, ibg;
    ifg = color_of_int (Int_val (fg));
    ibg = color_of_int (Int_val (bg));
    init_pair (Int_val (code), ifg, ibg);
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_red_in_black (value code) {
    CAMLparam1(code);
    init_pair (1, COLOR_RED, COLOR_BLACK);
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_use_color (value code, value vw, value b) {
    CAMLparam2(vw, b);
    if (Bool_val(b)) wattron(Win_custom(vw), COLOR_PAIR(Int_val(code)));
    else wattroff(Win_custom(vw), COLOR_PAIR(Int_val(code)));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_red (value vw, value b) {
    CAMLparam2(vw, b);
    if (Bool_val(b)) wattron(Win_custom(vw), COLOR_PAIR(1));
    else wattroff(Win_custom(vw), COLOR_PAIR(1));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_wdeleteln (value vw) {
    CAMLparam1(vw);
    wdeleteln (Win_custom(vw));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_isprint (value v) {
    CAMLparam1(v);
    CAMLreturn(Val_bool(isprint(Int_val(v))));
}

value
ncurs_CAML_keyup (value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_int (KEY_UP));
}

value
ncurs_CAML_keyright (value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_int (KEY_RIGHT));
}

value
ncurs_CAML_keyleft (value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_int (KEY_LEFT));
}

value
ncurs_CAML_keydown (value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_int (KEY_DOWN));
}

value
ncurs_CAML_keybackspace (value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_int (KEY_BACKSPACE));
}

value
ncurs_CAML_keydc (value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_int (KEY_DC));
}

value
ncurs_CAML_keynpage (value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_int (KEY_NPAGE));
}

value
ncurs_CAML_keyppage (value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_int (KEY_PPAGE));
}

value
ncurs_CAML_backspake (value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_int (KEY_BACKSPACE));
}

value
ncurs_CAML_resize (value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_int (KEY_RESIZE));
}

value
ncurs_CAML_error (value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_int (ERR));
}

value
ncurs_CAML_sigwinch_supported (value unit) {
    CAMLparam1(unit);
#ifdef SIGWINCH
    CAMLreturn(Val_bool (1));
#else
    CAMLreturn(Val_bool (0));
#endif
}
value
ncurs_CAML_sigwinch (value unit) {
    CAMLparam1(unit);
#ifdef SIGWINCH
    CAMLreturn(Val_int (SIGWINCH));
#else
    // failwith ("Compiled without SIGWINCH support");
    CAMLreturn (Val_int (0));
#endif
}

value
ncurs_CAML_wdelch (value vw) {
    CAMLparam1(vw);
    wdelch (Win_custom(vw));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_winsch (value vw, value vc) {
    CAMLparam2(vw, vc);
    winsch (Win_custom(vw), Int_val (vc));
    CAMLreturn (Val_unit);
}

value
ncurs_CAML_halfdelay (value vc) {
    CAMLparam1 (vc);
    halfdelay (Int_val (vc));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_sigwinch_handler (value vi) {
    CAMLparam1 (vi);
#ifndef _WIN32
    struct winsize size;
    if (ioctl (fileno(stdout), TIOCGWINSZ, &size) == 0) {
        resizeterm (size.ws_row, size.ws_col);
    }
#endif
    CAMLreturn (Val_unit);
}

value
ncurs_CAML_resize_handler (value vi) {
    CAMLparam1 (vi);
#ifndef _WIN32
    struct winsize size;
    if (ioctl (fileno(stdout), TIOCGWINSZ, &size) == 0) {
        resizeterm (size.ws_row, size.ws_col);
    }
#endif
    CAMLreturn (Val_unit);
}

value
ncurs_CAML_wclear (value vw) {
    CAMLparam1(vw);
    wclear (Win_custom(vw));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_werase (value vw) {
    CAMLparam1 (vw);
    werase (Win_custom(vw));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_touchwin (value vw) {
    CAMLparam1 (vw);
    touchwin (Win_custom(vw));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_clearok (value vw, value vtf) {
    CAMLparam2 (vw, vtf);
    clearok (Win_custom(vw), Bool_val(vtf));
    CAMLreturn(Val_unit);
}

value
ncurs_CAML_redrawwin (value vw) {
    CAMLparam1 (vw);
#ifndef _WIN32
    redrawwin (Win_custom(vw));
#endif
    CAMLreturn(Val_unit);
}

#endif
