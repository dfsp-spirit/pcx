# subbyte manipulation functions

#' @title Split each entry of 8 bit integer vector into 1, 2 or 4 bit integers.
#'
#' @param data_in integer vector, containing values in range 0..255L (8 bit unsigned int values).
#'
#' @param output_bits_per_int integer, one of 1L, 2L, 4L. The bits per output integer.
#'
#' @return vector of integer, the values range is smaller than the range of the 8 bit input values, and the number of values in greater.
#'
#' @keywords internal
uint8split <- function(data_in, output_bits_per_int = 4L) {
  if( ! output_bits_per_int %in% c(1L, 2L, 4L, 8L)) {
    stop("Parameter 'output_bits_per_int' must be one of 1L, 2L, 4L, 8L.");
  }
  #cat(sprintf("bitpix=%d\n", output_bits_per_int));
  #print(dim(data_in))
  #print(data_in)
  #print(which(is.na(data_in)))
  if((any(data_in > 255L) | any(data_in < 0L))) {
    stop("Parameter 'data_in' must contain only integers which can be represented as unsigned 8 bit integers, i.e., values in range 0..255.");
  }
  if(output_bits_per_int == 8L) {
    return(data_in);
  } else {
    if(output_bits_per_int == 1L) {
      return(ints.to.uint1(data_in));
    } else if(output_bits_per_int == 2L) {
      return(ints.to.uint2(data_in));
    } else { # 4L
      return(ints.to.uint4(data_in));
    }
  }
}


#' @title Extract subbits from integer as unsigned int.
#'
#' @description Extract bits \code{start_index} to \code{stop_index} from an 8 bit integer.
#'
#' @param int8 integer, must be in range 0L..255L. The integer is of course stored as a 32 bit integer internally in \code{R}, but it must contain a value between \code{0} and \code{255}, the other bits are ignored.
#'
#' @param start_index integer in range 1L..32L, the start bit index.
#'
#' @param stop_index integer in range 1L..32L, the stop bit index.
#'
#' @return an unsigned integer, the possible range depends on the number of bits.
#'
#' @note Thanks to Julian_Hn at stackoverflow for this function.
#'
#' @keywords internal
uint.subbits.as.uint <- function(int8, start_index, stop_index) {
  bits = intToBits(int8);
  res = packBits(c(bits[start_index:stop_index],
                   rep(as.raw(0), 32-(stop_index-start_index+1))), type = "integer");
  return(res);
}


#' @title Split n 8 bit unsigned integers into 2n 4 bit unsigned integers.
#'
#' @param int8_vec integer vector of \code{n} values, all of which must be in range 0..255L.
#'
#' @return \code{2n} integers in range 0..15L.
#' @keywords internal
ints.to.uint4 <- function(int8_vec) {
  res_lbits = sapply(int8_vec, uint.subbits.as.uint, 1L, 4L);
  res_rbits = sapply(int8_vec, uint.subbits.as.uint, 5L, 8L);
  return(c(rbind(res_lbits, res_rbits)));
}


#' @title Split n 8 bit unsigned integers into 4n 2 bit unsigned integers.
#'
#' @param int8_vec integer vector of \code{n} values, all of which must be in range 0..255L.
#'
#' @return \code{4n} integers in range 0..3L.
#' @keywords internal
ints.to.uint2 <- function(int8_vec) {
  res_bits1 = sapply(int8_vec, uint.subbits.as.uint, 1L, 2L);
  res_bits2 = sapply(int8_vec, uint.subbits.as.uint, 3L, 4L);
  res_bits3 = sapply(int8_vec, uint.subbits.as.uint, 5L, 6L);
  res_bits4 = sapply(int8_vec, uint.subbits.as.uint, 7L, 8L);
  return(c(rbind(res_bits1, res_bits2, res_bits3, res_bits4)));
}


#' @title Split n 8 bit unsigned integers into 8n 1 bit unsigned integers.
#'
#' @param int8_vec integer vector of \code{n} values, all of which must be in range 0..255L.
#'
#' @return \code{8n} integers in range 0..1L.
#' @keywords internal
ints.to.uint1 <- function(int8_vec) {
  res_bits1 = sapply(int8_vec, uint.subbits.as.uint, 1L, 1L);
  res_bits2 = sapply(int8_vec, uint.subbits.as.uint, 2L, 2L);
  res_bits3 = sapply(int8_vec, uint.subbits.as.uint, 3L, 3L);
  res_bits4 = sapply(int8_vec, uint.subbits.as.uint, 4L, 4L);
  res_bits5 = sapply(int8_vec, uint.subbits.as.uint, 5L, 5L);
  res_bits6 = sapply(int8_vec, uint.subbits.as.uint, 6L, 6L);
  res_bits7 = sapply(int8_vec, uint.subbits.as.uint, 7L, 7L);
  res_bits8 = sapply(int8_vec, uint.subbits.as.uint, 8L, 8L);
  return(c(rbind(res_bits1, res_bits2, res_bits3, res_bits4, res_bits5, res_bits6, res_bits7, res_bits8)));
}
