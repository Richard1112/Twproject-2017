<%@ page import="bsh.Interpreter,
                 bsh.TargetError,
                 org.jblooming.ontology.Identifiable,
                 org.jblooming.utilities.ExcelWriter,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.ReflectionUtilities,
                 org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.html.button.ButtonExportXLS,
                 org.jblooming.waf.view.PageState,
                 java.net.URLEncoder, java.util.ArrayList, java.util.List, org.jblooming.security.License" %><%
  if (License.assertLevel(20)) {
    PageState pageState = PageState.getCurrentPageState(request);
    ButtonExportXLS beXLS = (ButtonExportXLS) pageState.sessionState.getAttribute(ButtonExportXLS.class.getName() + "_" + pageState.getEntry("BUTTON_ID").stringValue());
    response.setContentType("application/vnd.ms-excel");


    String filename = beXLS.outputFileName == null ? "export" : beXLS.outputFileName + ".xls";

    String filenameEncoded = URLEncoder.encode(filename, "UTF8");
    filenameEncoded = StringUtilities.replaceAllNoRegex(StringUtilities.replaceAllNoRegex(filenameEncoded, "+", "_"), " ", "_");
    response.setHeader("Content-Disposition", "attachment;filename=\"" + filenameEncoded + "\";");

    ServletOutputStream sout = response.getOutputStream();
    ExcelWriter excelWriter = (null != beXLS.sheetName && !"".equals(beXLS.sheetName)) ? new ExcelWriter(sout, beXLS.sheetName) : new ExcelWriter(sout);

    beXLS.controller.perform(request, response);

    // print filter
    if (beXLS.filterFieldToMonitor.size() > 0) {
      String filterString = "";
      for (String key : beXLS.filterFieldToMonitor) {
        String value = pageState.getEntry(key).stringValueNullIfEmpty();
        if (JSP.ex(value)) {
          filterString = filterString + pageState.getI18n(key) + ":" + value + "; ";
        }
      }
      excelWriter.println(filterString);
    }

    // print header
    String[] riga = beXLS.fieldLabels.toArray(new String[0]);
    excelWriter.printHeader(riga);

    // print data
    List objs = null;
    if (beXLS.objectList.size() > 0) {
      objs = beXLS.objectList;
    } else {
      objs = pageState.getPage() != null ? pageState.getPage().getAllElements() : new ArrayList();
    }

    //define bsh interpreter
    Interpreter bsh = new Interpreter();
    bsh.getNameSpace().importClass("org.jblooming.utilities.JSP");
    bsh.getNameSpace().importClass("org.jblooming.utilities.DateUtilities");
    for (String importClass : beXLS.importClasses)
      bsh.getNameSpace().importClass(importClass);

    for (Object obj : objs) {
      List<Object> elemLst = new ArrayList<Object>();
      Object myObject = null;

      if (obj instanceof Object[]) {
        int i = 0;
        for (Object arrayMember : ((Object[]) obj)) {
          Object deproxiedArrayMember = ReflectionUtilities.getUnderlyingObject(arrayMember);
          if (deproxiedArrayMember instanceof Identifiable) {
            String objClass = ReflectionUtilities.deProxy(arrayMember.getClass().getName());
            bsh.set("gen" + i, deproxiedArrayMember);
            bsh.eval(beXLS.entityAlias + i + "=(" + objClass + ")gen" + i + "; ");
            i++;
          }
        }

      } else {
        myObject = obj;
        // fill bsh environment with value
        bsh.set("gen", myObject);
        String objClass = ReflectionUtilities.deProxy(myObject.getClass().getName());
        bsh.eval(beXLS.entityAlias + "=(" + objClass + ")gen; ");
      }

      for (String prop : beXLS.propertiesToExport) {

        String printValue = "";
        Object result = null;
        //value extraction
        try {
          if (prop.toUpperCase().startsWith("BSH:")) {
            String bshExpr = prop.substring(4);
            if (beXLS.debug) {
              result = bsh.eval(bshExpr);
            } else {
              try {
                result = bsh.eval(bshExpr);
              } catch (Throwable t) {
              }
            }
          } else {
            if (beXLS.debug) {
              result = ReflectionUtilities.getFieldValue(prop, myObject);
            } else {
              try {
                result = ReflectionUtilities.getFieldValue(prop, myObject);
              } catch (Throwable t) {
              }
            }
          }
        } catch (TargetError bshte) {
          if (!(bshte.getTarget() instanceof NullPointerException)) {
            throw bshte;
          }

        }
        elemLst.add(result == null ? "" : result);
      }

      Object[] rigao = elemLst.toArray(new Object[0]);
      excelWriter.println(rigao);
    }

    excelWriter.generate();
    sout.close();
  }
%>