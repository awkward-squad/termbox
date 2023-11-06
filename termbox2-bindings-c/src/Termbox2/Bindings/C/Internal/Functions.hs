module Termbox2.Bindings.C.Internal.Functions
  ( tb_attr_width,
    tb_cell_buffer,
    tb_clear,
    tb_extend_cell,
    tb_get_fds,
    tb_has_egc,
    tb_has_truecolor,
    tb_height,
    tb_hide_cursor,
    tb_init,
    tb_init_fd,
    tb_init_file,
    tb_init_rwfd,
    tb_invalidate,
    tb_last_errno,
    tb_peek_event,
    tb_poll_event,
    tb_present,
    tb_print,
    tb_print_ex,
    tb_printf,
    tb_printf_ex,
    tb_send,
    tb_sendf,
    tb_set_cell,
    tb_set_cell_ex,
    tb_set_clear_attrs,
    tb_set_cursor,
    tb_set_func,
    tb_set_input_mode,
    tb_set_output_mode,
    tb_shutdown,
    tb_strerror,
    tb_utf8_char_length,
    tb_utf8_char_to_unicode,
    tb_utf8_unicode_to_char,
    tb_version,
    tb_width,
  )
where

-- int tb_attr_width(void);
-- int tb_clear(void);
-- int tb_extend_cell(int x, int y, uint32_t ch);
-- int tb_get_fds(int *ttyfd, int *resizefd);
-- int tb_has_egc(void);
-- int tb_has_truecolor(void);
-- int tb_height(void);
-- int tb_hide_cursor(void);
-- int tb_init(void);
-- int tb_init_fd(int ttyfd);
-- int tb_init_file(const char *path);
-- int tb_init_rwfd(int rfd, int wfd);
-- int tb_invalidate(void);
-- int tb_last_errno(void);
-- int tb_peek_event(struct tb_event *event, int timeout_ms);
-- int tb_poll_event(struct tb_event *event);
-- int tb_present(void);
-- int tb_print(int x, int y, uintattr_t fg, uintattr_t bg, const char *str);
-- int tb_print_ex(int x, int y, uintattr_t fg, uintattr_t bg, size_t *out_w, const char *str);
-- int tb_printf(int x, int y, uintattr_t fg, uintattr_t bg, const char *fmt, ...);
-- int tb_printf_ex(int x, int y, uintattr_t fg, uintattr_t bg, size_t *out_w, const char *fmt, ...);
-- int tb_send(const char *buf, size_t nbuf);
-- int tb_sendf(const char *fmt, ...);
-- int tb_set_cell(int x, int y, uint32_t ch, uintattr_t fg, uintattr_t bg);
-- int tb_set_cell_ex(int x, int y, uint32_t *ch, size_t nch, uintattr_t fg, uintattr_t bg);
-- int tb_set_clear_attrs(uintattr_t fg, uintattr_t bg);
-- int tb_set_cursor(int cx, int cy);
-- int tb_set_func(int fn_type, int (*fn)(struct tb_event *, size_t *));
-- int tb_set_input_mode(int mode);
-- int tb_set_output_mode(int mode);
-- int tb_shutdown(void);
-- const char *tb_strerror(int err);
-- int tb_utf8_char_length(char c);
-- int tb_utf8_char_to_unicode(uint32_t *out, const char *c);
-- int tb_utf8_unicode_to_char(char *out, uint32_t c);
-- const char *tb_version(void);
-- int tb_width(void);
-- struct tb_cell *tb_cell_buffer(void);

tb_attr_width :: ()
tb_attr_width = undefined

tb_cell_buffer :: ()
tb_cell_buffer = undefined

tb_clear :: ()
tb_clear = undefined

tb_extend_cell :: ()
tb_extend_cell = undefined

tb_get_fds :: ()
tb_get_fds = undefined

tb_has_egc :: ()
tb_has_egc = undefined

tb_has_truecolor :: ()
tb_has_truecolor = undefined

tb_height :: ()
tb_height = undefined

tb_hide_cursor :: ()
tb_hide_cursor = undefined

tb_init :: ()
tb_init = undefined

tb_init_fd :: ()
tb_init_fd = undefined

tb_init_file :: ()
tb_init_file = undefined

tb_init_rwfd :: ()
tb_init_rwfd = undefined

tb_invalidate :: ()
tb_invalidate = undefined

tb_last_errno :: ()
tb_last_errno = undefined

tb_peek_event :: ()
tb_peek_event = undefined

tb_poll_event :: ()
tb_poll_event = undefined

tb_present :: ()
tb_present = undefined

tb_print :: ()
tb_print = undefined

tb_print_ex :: ()
tb_print_ex = undefined

tb_printf :: ()
tb_printf = undefined

tb_printf_ex :: ()
tb_printf_ex = undefined

tb_send :: ()
tb_send = undefined

tb_sendf :: ()
tb_sendf = undefined

tb_set_cell :: ()
tb_set_cell = undefined

tb_set_cell_ex :: ()
tb_set_cell_ex = undefined

tb_set_clear_attrs :: ()
tb_set_clear_attrs = undefined

tb_set_cursor :: ()
tb_set_cursor = undefined

tb_set_func :: ()
tb_set_func = undefined

tb_set_input_mode :: ()
tb_set_input_mode = undefined

tb_set_output_mode :: ()
tb_set_output_mode = undefined

tb_shutdown :: ()
tb_shutdown = undefined

tb_strerror :: ()
tb_strerror = undefined

tb_utf8_char_length :: ()
tb_utf8_char_length = undefined

tb_utf8_char_to_unicode :: ()
tb_utf8_char_to_unicode = undefined

tb_utf8_unicode_to_char :: ()
tb_utf8_unicode_to_char = undefined

tb_version :: ()
tb_version = undefined

tb_width :: ()
tb_width = undefined
