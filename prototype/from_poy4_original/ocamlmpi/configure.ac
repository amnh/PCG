#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.60)
AC_INIT(FULL-PACKAGE-NAME, VERSION, BUG-REPORT-ADDRESS)
AC_CONFIG_SRCDIR([camlmpi.h])
AC_CONFIG_HEADER([config.h])

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL

# What are we going to substitute in the Makefile
AC_SUBST(USEGRAPHICS)
AC_SUBST(USEINTERFACE)
AC_SUBST(USESHAREDHD)
AC_SUBST(USEPARALLEL)
AC_SUBST(OCAML)
AC_SUBST(OCAMLC)
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLDEP)
AC_SUBST(OCAMLTOP)
AC_SUBST(LIBS)
AC_SUBST(OCAMLFLAGS)

AC_ARG_WITH(ocaml-flags, [Add the following flags to the ocaml compiler.],
            [OCAMLFLAGS=$withval],[])


AC_CHECK_PROGS([OCAML], [ocaml])
AC_CHECK_PROGS([OCAMLC], [ocamlc.opt ocamlc])
AC_CHECK_PROGS([OCAMLOPT], [ocamlopt.opt ocamlopt])
AC_CHECK_PROGS([OCAMLDEP], [ocamldep])
AC_CHECK_PROGS([OCAMLTOP], [ocamlmktop])

# Checks for libraries.

AC_ARG_ENABLE([mpi], [AS_HELP_STRING([--enable-mpi], 
              [Turns on parallel support using MPI. When using MPI remember to set CC to the appropriate mpicc compiler.])], 
              [USEPARALLEL=true], 
              [USEPARALLEL=false])

# System dependent options
case `../gnu/config.guess` in
    x86_64*)
        OCAMLFLAGS="-fno-PIC $OCAMLFLAGS" 
        ;;
    *-apple-*)
        CFLAGS="-I /usr/include/malloc $CFLAGS"
        ;;
    *-cygwin*)
        USEWIN32=true
        CFLAGS="-mwin32 -mno-cygwin $CFLAGS"
        ;;
    *)
        ;;
esac

#AC_MSG_CHECKING([Checking for MPI usability])
#AC_LANG_CONFTEST([AC_LANG_PROGRAM([],[[MPI_Init()]])])
#$CC -E -dD conftest.c -o - 
#AC_MSG_RESULT([The results of the test is $?])
#if [ ! $? ]; then
#    AC_MSG_FAILURE([I could not compile the MPI test program with your configuration.])
#fi

#AS_IF([test $USEPARALLEL = "true"], [AC_CHECK_LIB([$MPILNAME], [MPI_Init],
#      [LIBS="-l$MPILNAME $LIBS"], [AC_MSG_FAILURE([In order to compile the parallel version, an implemention of MPI is required. However, I failed while checking for $MPILNAME.])])])
# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_HEADER_STDC

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
