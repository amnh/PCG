#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "config.h"
#ifdef HAVE_XSLT
#include <stdio.h>
#include <string.h>
#include <libxml/xmlmemory.h>
#include <libxml/debugXML.h>
#include <libxml/HTMLtree.h>
#include <libxml/xmlIO.h>
#include <libxml/xinclude.h>
#include <libxml/catalog.h>
#include <libxslt/xslt.h>
#include <libxslt/xsltInternals.h>
#include <libxslt/transform.h>
#include <libxslt/xsltutils.h>

value
caml_XSLT_process (value xml_file, value xslt_file, value output_file) {
    CAMLparam3(xml_file, xslt_file, output_file);
    xsltStylesheetPtr c_xslt_file;
    xmlDocPtr c_xml_file, c_output_file;
    const char *params[1];
    FILE *fl;
    params[0]=NULL;
    c_xslt_file = xsltParseStylesheetFile((const xmlChar *) String_val(xslt_file));
    c_xml_file = xmlParseFile(String_val(xml_file));
    c_output_file = xsltApplyStylesheet(c_xslt_file, c_xml_file, params);
    fl = fopen (String_val(output_file), "a");
    xsltSaveResultToFile(fl, c_output_file, c_xslt_file);
    fclose(fl);
    xsltFreeStylesheet(c_xslt_file);
    xmlFreeDoc(c_xml_file);
    xmlFreeDoc(c_output_file);
    xsltCleanupGlobals();
    xmlCleanupParser();
    CAMLreturn(Val_unit);
}
#endif
