extern void inner_caf_error_stop_integer(void *);

extern void caf_error_stop_integer_c(void *stop_code) {
    inner_caf_error_stop_integer(stop_code);
}
