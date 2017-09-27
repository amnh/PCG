#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "caml/mlvalues.h"
#include "caml/fail.h"
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/callback.h"

#include <zlib.h>


/* ERROR REPORTING */
/* raise the Caml exception */
static void raise_mlgz_exn(const char *msg)
{
  static value * exn = NULL;
  if(exn == NULL)
    exn = caml_named_value ("mlgz_exn");
  raise_with_string(*exn, (char *)msg) ;
}


/* raise the library exception or the Sys_error exn */
static void mlgz_error(gzFile file)
{
  int errnum;
  const char *msg;
  msg = gzerror(file, &errnum);
  if(errnum < 0){
    gzclose(file) ;
    switch(errnum){
    case Z_ERRNO :
      raise_sys_error(copy_string(strerror(errno))) ;
    case Z_MEM_ERROR :
      raise_out_of_memory() ;
    default :
      raise_mlgz_exn(msg) ;
    }
  }
}

static value concat_strings(const char *s1, const char *s2)
{
  int l1 = strlen(s1) ;
  int l2 = strlen(s2) ;
  value res = alloc_string(l1+l2+2);
  memcpy(String_val(res), s1, l1) ;
  Byte(res, l1)   = ':' ;
  Byte(res, l1+1) = ' ' ;
  memcpy(String_val(res)+l1+2, s2, l2) ;
  return res ;
}

/* Wrapping the C pointer to a gzFile in a Caml abstract_value
   to avoid GC potential problems */
static value Val_ptr(gzFile ptr)
{
  value result = alloc_small(1, Abstract_tag);
  Field(result, 0) = (value) ptr ;
  return result ;
}

/* extract the gzFile from the Caml value */
#define Gzfile_val(v)  ((gzFile)(Field((v), 0)))

/* get library version */
value mlgz_zlibversion(value unit)
{
  return caml_copy_string(zlibVersion());
}

/* standard stub code now ... */


value mlgz_gzopen_gen(value name, value mode)
{
  gzFile str;
  str = gzopen(String_val(name), String_val(mode)) ;
  if(str==NULL){
    if(errno==0)
      raise_out_of_memory();
    else
      raise_sys_error(concat_strings(String_val(name), strerror(errno)));
  }
  return Val_ptr(str);
}

value mlgz_gzsetparams(value chan, value comp, value strat)
{
  static const int c_strat[] = {
    Z_DEFAULT_STRATEGY, Z_FILTERED, Z_HUFFMAN_ONLY } ;
  gzFile str = Gzfile_val(chan) ;
  int res ;
  res = gzsetparams(str, Int_val(comp), c_strat[ Int_val(strat) ]);
  if(res<0)
    mlgz_error(str);
  return Val_unit;
}

value mlgz_gzread(value chan, value buf, value pos, value len)
{
  gzFile str=Gzfile_val(chan);
  char *c_buf;
  int res;

  if((Int_val(len) + Int_val(pos) > string_length(buf))
     || (Int_val(len) < 0)
     || (Int_val(pos) < 0))
    invalid_argument("Gz.read") ;
  c_buf = String_val(buf) + Int_val(pos) ;
  res = gzread(str, c_buf, Int_val(len));
  if(res<0)
    mlgz_error(str);
  return Val_int(res);
}

value mlgz_gzwrite(value chan, value buf, value pos, value len)
{
  gzFile str=Gzfile_val(chan);
  char *c_buf;
  int res;

  if((Int_val(len) + Int_val(pos) > string_length(buf))
     || (Int_val(len) < 0)
     || (Int_val(pos) < 0))
    invalid_argument("Gz.write") ;
  c_buf = String_val(buf) + Int_val(pos) ;
  res = gzwrite(str, c_buf, Int_val(len));
  if(res==0)
    mlgz_error(str);
  return Val_unit;
}

value mlgz_gzputs(value chan, value buf)
{
  gzFile str=Gzfile_val(chan);
  int res;

  res = gzwrite(str, String_val(buf), string_length(buf));
  if(res<0)
    mlgz_error(str);
  return Val_unit;
}


value mlgz_input_scan_line(value chan, value buf)
{
  gzFile str=Gzfile_val(chan);
  int len = string_length(buf);
  char *c_buf= String_val(buf);

  if(gzeof(str))
    raise_end_of_file();
  if((gzgets(str, c_buf, len))==Z_NULL)
    mlgz_error(str);
  return Val_int(strlen(c_buf));
}

value mlgz_gzputc(value chan, value c)
{
  gzFile str=Gzfile_val(chan);
  int res;

  res = gzputc(str, Int_val(c));
  if(res<0)
    mlgz_error(str);
  return Val_unit;
}

value mlgz_gzgetc(value chan)
{
  gzFile str=Gzfile_val(chan);
  int res ;
  res = gzgetc(Gzfile_val(chan));
  if(res<0){
    if(gzeof(str))
      raise_end_of_file();
    else
      mlgz_error(str);
  }
  return Val_int(res);
}

value mlgz_gzrewind(value chan)
{
  gzFile str=Gzfile_val(chan);
  if((gzrewind(str)) < 0)
    mlgz_error(str);
  return Val_unit;
}

value mlgz_gzclose(value chan)
{
  gzFile str=Gzfile_val(chan);
  int res;
  res = gzclose(str);
  if(res<0)
    mlgz_error(str);
  {
    void **p = (void **) chan;
    *p = NULL;
  }
  return Val_unit;
}

value mlgz_gzflush(value flush, value chan)
{
  static const int flush_val[] = {
    Z_SYNC_FLUSH, Z_FULL_FLUSH, Z_FINISH } ;
  gzFile str = Gzfile_val(chan);
  int c_flush_val = 0;
  int res;
  if(Is_block(flush))
    c_flush_val = Int_val(Field(flush, 0));
  res = gzflush(str, flush_val[ c_flush_val ]) ;
  if(res<0)
    mlgz_error(str);
  return Val_unit;
}

value mlgz_gzseek(value chan, value offset, value out_chan)
{
  gzFile str = Gzfile_val(chan);
  int c_offset = Int_val(offset);
  int res;
  if(Bool_val(out_chan) && c_offset < 0)
    invalid_argument("Gz.seek_out");
  res = gzseek(str, c_offset, SEEK_SET);
  if(res<0)
    mlgz_error(str);
  return Val_unit;
}

value mlgz_gztell(value chan)
{
  gzFile str = Gzfile_val(chan);
  int res ;
  res = gztell(str);
  if(res<0)
    mlgz_error(str);
  return Val_unit;
}


/* IN MEMORY FUNCTIONS */
value mlgz_compress(value v_lvl, value v_src, value v_pos, value v_len)
{
  value v_ret;
  int pos, len, out_buf_len, r;
  int level = Z_BEST_COMPRESSION ;
  uLong out_len;
  const unsigned char *in_buf;
  unsigned char *out_buf;

  if(Is_block(v_lvl))
    level = Int_val(Field(v_lvl, 0)) ;
  pos = Int_val(v_pos);
  len = Int_val(v_len);
  in_buf = (unsigned char *) String_val(v_src) + pos;
  if(level < 0 || level > 9
     || pos < 0 || len < 0
     || pos + len > string_length(v_src))
    invalid_argument("Gz.compress");
  out_buf_len = len + 12;
  out_buf_len += out_buf_len / 1000;
  out_buf = malloc(out_buf_len);
  if(out_buf == NULL)
    raise_out_of_memory();
  while(1) {
    out_len = out_buf_len;
    r = compress2(out_buf, &out_len, in_buf, len, level);
    if(r == Z_OK) {
      break;
    } else if(r == Z_BUF_ERROR) {
      unsigned char *new_buf;

      out_buf_len *= 2;
      new_buf = realloc(out_buf, out_buf_len);
      if(new_buf == NULL) {
	free(out_buf);
	raise_out_of_memory();
      }
      out_buf = new_buf;
    } else {
      free(out_buf);
      raise_out_of_memory();
    }
  }
  v_ret = alloc_string(out_len);
  memcpy(String_val(v_ret), out_buf, out_len);
  free(out_buf);
  return v_ret ;
}

value mlgz_uncompress(value v_src, value v_pos, value v_len)
{
  value v_ret;
  int pos, len, out_buf_len, r;
  uLong out_len;
  const unsigned char *in_buf;
  unsigned char *out_buf;

  pos = Int_val(v_pos);
  len = Int_val(v_len);
  in_buf = (unsigned char *) String_val(v_src) + pos;
  if(pos < 0 || len < 0 || pos + len > string_length(v_src))
    invalid_argument("Gz.uncompress");
  out_buf_len = len * 2;
  out_buf = malloc(out_buf_len);
  if(out_buf == NULL)
    raise_out_of_memory();
  while(1) {
    out_len = out_buf_len;
    r = uncompress(out_buf, &out_len, in_buf, len);
    if(r == Z_OK) {
      break;
    } else if(r == Z_BUF_ERROR) {
      unsigned char *new_buf;

      out_buf_len *= 2;
      new_buf = realloc(out_buf, out_buf_len);
      if(new_buf == NULL) {
	free(out_buf);
	raise_out_of_memory();
      }
      out_buf = new_buf;
    } else if(r == Z_MEM_ERROR) {
      free(out_buf);
      raise_out_of_memory();
    } else {
      free(out_buf);
      raise_mlgz_exn("uncompress");
    }
  }
  v_ret = alloc_string(out_len);
  memcpy(String_val(v_ret), out_buf, out_len);
  free(out_buf);
  return v_ret ;
}
