<%@ page import="bsh.Interpreter,
                 com.Ostermiller.util.ExcelCSVPrinter,
                 org.jblooming.utilities.DateUtilities,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.ReflectionUtilities,
                 org.jblooming.waf.SessionState,
                 org.jblooming.waf.html.button.ButtonExportCSV,
                 org.jblooming.waf.view.PageState,
                 java.io.OutputStreamWriter,
                 java.nio.charset.Charset,
                 java.util.ArrayList,
                 java.util.Date,
                 java.util.List,
                 java.util.Locale" %><%

  PageState pageState = PageState.getCurrentPageState(request);
  ButtonExportCSV beCSV = (ButtonExportCSV) pageState.sessionState.getAttribute(ButtonExportCSV.class.getName());
  response.setContentType("application/vnd.ms-excel");
  response.setHeader("Content-Disposition", "attachment;filename=\"" + beCSV.outputFileName + "." + beCSV.outputFileExt +"\";");

  Charset charset = Charset.forName("UTF-8");
  OutputStreamWriter osw = new OutputStreamWriter(response.getOutputStream(), charset);
  ExcelCSVPrinter csvPrinter = new ExcelCSVPrinter(osw);

  String language = SessionState.getLocale().getLanguage();

  boolean isItalian = Locale.ITALY.getLanguage().equalsIgnoreCase(language);

  if (isItalian) {
    csvPrinter.changeDelimiter(';');
  }

  if(JSP.ex(beCSV.delimiter)){
    csvPrinter.changeDelimiter(beCSV.delimiter);
  }
  
  beCSV.controller.perform(request, response);

  // print filter
  if (beCSV.filterFieldToMonitor.size() > 0) {
    String filterString = "";
    for (String key : beCSV.filterFieldToMonitor) {
      String value = pageState.getEntry(key).stringValueNullIfEmpty();
      if (JSP.ex(value)) {
        filterString = filterString + pageState.getI18n(key) + ":" + value + "; ";
      }
    }
    csvPrinter.println(filterString);
  }

  // print header
  String[] riga = beCSV.fieldLabels.toArray(new String[0]);
  csvPrinter.println(riga);

  // print data
  List objs = pageState.getPage().getAllElements();

  //define bsh interpreter
  Interpreter bsh = new Interpreter();
  bsh.getNameSpace().importClass("org.jblooming.utilities.JSP");
  
  for (Object obj : objs) {
    List<String> elemLst = new ArrayList<String>();

    // fill bsh environment with value
    bsh.set("gen", obj);
    String objClass = ReflectionUtilities.deProxy(obj.getClass().getName());
    bsh.eval(beCSV.entityAlias + "=(" + objClass + ")gen; ");

    for (String prop : beCSV.propertiesToExport) {

      String printValue = "";
      Object result = null;

      //value extraction
      if (prop.toUpperCase().startsWith("BSH:")) {
        String bshExpr = prop.substring(4);
        result = bsh.eval(bshExpr);
      } else {
        result = ReflectionUtilities.getFieldValue(prop, obj);
      }

      if (result instanceof String) {
        printValue = (String) result;

      } else if (result instanceof Integer) {
        printValue = JSP.w(result);

      } else if (result instanceof Date) {
        Date dateObj = (Date) result;
        if (isItalian) {
          printValue = DateUtilities.dateToString(dateObj, "dd/MM/yyyy");
        } else {
          printValue = DateUtilities.dateToString(dateObj, "MM/dd/yyyy");
        }
      } else if (result instanceof Boolean) {
        Boolean boolValue = (Boolean) result;
        printValue = boolValue.toString();
      }
      elemLst.add(printValue);
    }

    riga = elemLst.toArray(new String[0]);
    csvPrinter.println(riga);
  }
  osw.close();

%>