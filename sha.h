#include <inttypes.h>
#include <stdio.h>
int sha256(uint8_t *inp, ssize_t len, uint8_t out[32]);
int sha512(uint8_t *inp, ssize_t len, uint8_t out[64]);
