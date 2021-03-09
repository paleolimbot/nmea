#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <stdlib.h>

void nmea_checksum(unsigned char* bytes, int len, 
                   int* calc, int* found, int* valid_start, int* valid_end) {
    // find the `valid_end` by searching for the asterisk from the end
    int check_end = len;
    for (int i = len - 1; i >= 0; i--) {
        if (bytes[i] == '*') {
            check_end = i;
            break;
        }
    }

    if (check_end == len) {
        *valid_end = NA_INTEGER;
    } else {
        *valid_end = check_end;
    }

    // find valid start by searching for $ from the start
    int check_start = 0;
    for (int i = 0; i < check_end; i++) {
        if (bytes[i] == '$') {
            check_start = i + 1;
            break;
        }
    }

    if (check_start == 0) {
        *valid_start = NA_INTEGER;
    } else {
        *valid_start = check_start;
    }

    // calculate the checksum
    int chk = 0;
    for (int i = check_start; i < check_end; i++) {
        chk ^= bytes[i];
    }
    *calc = chk;

    // parse the hexadecimal checksum off of the end if possible
    if ((len - check_end) >= 3) {
        char hex_buf[3];
        hex_buf[0] = bytes[check_end + 1];
        hex_buf[1] = bytes[check_end + 2];
        hex_buf[2] = '\0';

        char* end_ptr;
        int chk_val = strtol(hex_buf, &end_ptr, 16);

        if ((end_ptr - hex_buf) == 2) {
            *found = chk_val;
        } else {
            *found = NA_INTEGER;
        }
    } else {
        *found = NA_INTEGER;
    }
}

SEXP nmea_c_checksum(SEXP nmea) {
    R_xlen_t size = Rf_xlength(nmea);
    SEXP calc = PROTECT(Rf_allocVector(INTSXP, size));
    SEXP found = PROTECT(Rf_allocVector(INTSXP, size));
    SEXP valid_start = PROTECT(Rf_allocVector(INTSXP, size));
    SEXP valid_end = PROTECT(Rf_allocVector(INTSXP, size));

    int* pcalc = INTEGER(calc);
    int* pfound = INTEGER(found);
    int* pvalid_start = INTEGER(valid_start);
    int* pvalid_end = INTEGER(valid_end);

    SEXP item;
    for (R_xlen_t i = 0; i < size; i++) {
        item = VECTOR_ELT(nmea, i);
        if (item == R_NilValue) {
            pcalc[i] = NA_INTEGER;
            pfound[i] = NA_INTEGER;
            pvalid_start[i] = NA_INTEGER;
            pvalid_end[i] = NA_INTEGER;
        } else {
            nmea_checksum(
                RAW(item),
                Rf_length(item),
                pcalc + i,
                pfound + i,
                pvalid_start + i,
                pvalid_end + i
            );
        }   
    }

    const char* names[] = {"calc", "found", "start", "end", ""};
    SEXP result = PROTECT(Rf_mkNamed(VECSXP, names));
    SET_VECTOR_ELT(result, 0, calc);
    SET_VECTOR_ELT(result, 1, found);
    SET_VECTOR_ELT(result, 2, valid_start);
    SET_VECTOR_ELT(result, 3, valid_end);
    UNPROTECT(5);
    return result;
}
