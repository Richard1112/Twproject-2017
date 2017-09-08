<%@ page import="org.jblooming.designer.DesignerField, org.jblooming.ontology.Identifiable, org.jblooming.ontology.LookupSupport, org.jblooming.ontology.PersistentFile,
                 org.jblooming.persistence.PersistenceHome, org.jblooming.remoteFile.Document, org.jblooming.utilities.*, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.display.MultimediaFile, org.jblooming.waf.html.input.*, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, java.util.Currency,
                 java.util.Date, java.util.List"%><%

  
PageState pageState = PageState.getCurrentPageState(request);
DesignerField.Drawer fd = (DesignerField.Drawer) JspIncluderSupport.getCurrentInstance(request);
DesignerField designerField = fd.designerField;

String name =  designerField.name;
String separator = (designerField.separator == null) ? "</td><td>" : designerField.separator;
Class type = Class.forName(designerField.kind);
List classes = ReflectionUtilities.getInheritedClasses(type);

  // -----------------  SMART COMBO DEFINED BY USER ------------------------
if (type.equals(SmartCombo.class) && designerField.smartCombo != null) {
  if(designerField.exportable) {
    String value = designerField.smartCombo.getTextValue(pageState);
    if (JSP.ex(designerField.label)){
      %><label><%=designerField.label%></label><%
    }
    %><%=separator%><%=JSP.encode(value)%><%
  } else {

    designerField.smartCombo.label = designerField.label;
    designerField.smartCombo.innerLabel = designerField.innerLabel;
    designerField.smartCombo.readOnly = designerField.readOnly;//designer.readOnly || designerField.readOnly;
    designerField.smartCombo.required = designerField.required;
    designerField.smartCombo.separator = separator;
    designerField.smartCombo.preserveOldValue=designerField.preserveOldValue;
    designerField.smartCombo.onValueSelectedScript=designerField.additionalScript;
    if(designerField.maxLength > 0)
      designerField.smartCombo.fieldSize = designerField.maxLength;
    designerField.smartCombo.toHtml(pageContext);
  }

// -----------------  SQL COMBO DEFINED BY USER ------------------------
} else if (type.equals(SQLCombo.class) && designerField.sqlCombo != null) {
  if(designerField.exportable) {
    String value = designerField.sqlCombo.getTextValue(pageState);
    if (JSP.ex(designerField.label)){
      %><label><%=designerField.label%></label><%
    }
    %><%=separator%><%=JSP.encode(value)%><%
  } else {
    designerField.sqlCombo.label = designerField.label;
    designerField.sqlCombo.innerLabel = designerField.innerLabel;
    designerField.sqlCombo.readOnly = designerField.readOnly;//designer.readOnly || designerField.readOnly;
    designerField.sqlCombo.required = designerField.required;
    designerField.sqlCombo.separator = separator;
    designerField.sqlCombo.preserveOldValue=designerField.preserveOldValue;
    designerField.sqlCombo.onValueSelectedScript=designerField.additionalScript;
    if(designerField.maxLength > 0)
      designerField.sqlCombo.fieldSize = designerField.maxLength;
    designerField.sqlCombo.toHtml(pageContext);
  }

// -----------------  JSPHELPER DEFINED BY USER ------------------------
} else if(designerField.jspHelper!=null){
  if(!designerField.exportable){//!designer.exportable){
    designerField.jspHelper.id=name;
    designerField.jspHelper.toHtml(pageContext);
  }
// -----------------  STRING  ------------------------
} else if (type.equals(String.class)) {
  if(designerField.exportable) {
    String value = pageState.getEntry(designerField.name).stringValue();
    if (JSP.ex(designerField.label)){
      %><label><%=designerField.label%></label><%
    }
    %><%=separator%><%=JSP.encode(value)%><%
  } else {
    if (designerField.rowsLength > 1 || (designerField.maxLength > 80 && designerField.fieldSize < 1)) { //se non è stata inserita una formattazione per il testo
      int rows = 5;                                                                                      //se la fieldSize supera 80 si crea una text area 5X80
      int cols = 80;
      if (designerField.rowsLength > 1)
        rows = designerField.rowsLength;
      if (designerField.fieldSize > 0)
        cols = designerField.fieldSize;
      TextArea ta = new TextArea(designerField.label, name, separator, cols, rows, "", false, false, designerField.script);
      ta.innerLabel = designerField.innerLabel;
      ta.readOnly = designerField.readOnly;
      ta.required = designerField.required;
      if (designerField.autoSize)
        ta.setAutosize(50,300,20);
      ta.preserveOldValue=designerField.preserveOldValue;

      ta.toHtml(pageContext);
    } else {
      int size = 40;    //senza info esterse in textField è lungo 40
      if (designerField.fieldSize > 0)
        size = designerField.fieldSize;
      else if (designerField.maxLength > 0)
        size = designerField.maxLength; //se non ci sono info su fieldSize ma ci sono sul fieldSize si usano queste
      TextField tf = new TextField(designerField.label, name, separator,size, designerField.readOnly);
      if(designerField.maxLength>0)
        tf.maxlength = designerField.maxLength;
      tf.innerLabel = designerField.innerLabel;
      tf.autoSize = designerField.autoSize;
      tf.required = designerField.required;
      tf.fieldSize = designerField.fieldSize;
      tf.script=designerField.script;
      tf.preserveOldValue=designerField.preserveOldValue;

      tf.toHtml(pageContext);
    }
  }

// -----------------  DATE  ------------------------
} else if (classes.contains(Date.class)) {
  if(designerField.exportable) {
    String value = pageState.getEntry(designerField.name).stringValue();
    if (JSP.ex(designerField.label)){
      %><label><%=designerField.label%></label><%
    }
    %><%=separator%><%=JSP.encode(value)%><%
  } else {
    DateField datefield = new DateField(name);
    datefield.size = 10;
    datefield.setSearchField(false);
    datefield.labelstr = designerField.label;
    datefield.innerLabel = designerField.innerLabel;
    datefield.separator = separator;
    datefield.readOnly = designerField.readOnly;
    datefield.required = designerField.required;
    datefield.preserveOldValue=designerField.preserveOldValue;
    datefield.onblurOnDateValid=designerField.additionalScript;

    datefield.toHtml(pageContext);
  }

// -----------------  INTEGER  ------------------------
} else if (classes.contains(Integer.class) || classes.contains(int.class)) {
  if(designerField.exportable) {
    String value = pageState.getEntry(designerField.name).stringValue();
    if (JSP.ex(designerField.label)){
      %><label><%=designerField.label%></label><%
    }
    %><%=separator%><%=JSP.encode(value)%><%
  } else {
    int columnLength = designerField.fieldSize;
    if (classes.contains(int.class)) columnLength = 4;
    TextField tf = TextField.getIntegerInstance(name);
    tf.separator=separator;
    tf.label = designerField.label;
    tf.innerLabel = designerField.innerLabel;
    tf.readOnly = designerField.readOnly;
    tf.required = designerField.required;
    if(designerField.maxLength > 0)
      tf.maxlength = designerField.maxLength;
    tf.fieldSize = columnLength;
    tf.script = designerField.script;
    tf.preserveOldValue=designerField.preserveOldValue;
    tf.toHtml(pageContext);
  }

// -----------------  DOUBLE  ------------------------
} else if (classes.contains(Double.class)  || classes.contains(double.class)) {
  if(designerField.exportable) {
    String value = pageState.getEntry(designerField.name).stringValue();
    if (JSP.ex(designerField.label)){
      %><label><%=designerField.label%></label><%
    }
    %><%=separator%><%=JSP.encode(value)%><%
  } else {
    int columnLength = designerField.fieldSize;
    if (classes.contains(int.class)) columnLength = 4;
    TextField tf = TextField.getDoubleInstance(name);
    tf.separator=separator;
    tf.label = designerField.label;
    tf.innerLabel = designerField.innerLabel;
    tf.readOnly = designerField.readOnly;
    tf.required = designerField.required;
    if(designerField.maxLength > 0)
      tf.maxlength = designerField.maxLength;
    tf.fieldSize = columnLength;
    tf.script = designerField.script;
    tf.preserveOldValue=designerField.preserveOldValue;
    tf.toHtml(pageContext);
  }

// -----------------  CURRENCY  ------------------------
} else if (classes.contains(Currency.class)) {
  if(designerField.exportable) {
    String value = pageState.getEntry( designerField.name).stringValue();
    if (JSP.ex(designerField.label)){
      %><label><%=designerField.label%></label><%
    }
    %><%=separator%><%=JSP.encode(value)%><%
  } else {
    int columnLength = designerField.fieldSize;
    if (classes.contains(int.class)) columnLength = 4;
    TextField tf = TextField.getCurrencyInstance(name);
    tf.separator=separator;
    tf.label = designerField.label;
    tf.innerLabel = designerField.innerLabel;
    tf.readOnly = designerField.readOnly;
    tf.required = designerField.required;
    if(designerField.maxLength > 0)
      tf.maxlength = designerField.maxLength;
    tf.fieldSize = columnLength;
    tf.script = designerField.script;
    tf.preserveOldValue=designerField.preserveOldValue;

    tf.toHtml(pageContext);
  }

  //------------- Document  ----------------
} else if (classes.contains(Document.class)) {
  // recupero l'id del file storage
  String docContent = pageState.getEntry( designerField.name).stringValueNullIfEmpty();
  if (docContent == null)
    docContent = designerField.initialValue;
  String referralObjectId = null;
  if (docContent != null && docContent.startsWith("RF")) {
    String string = docContent.substring(2);
    String[] valori = string.split(":");
    if (valori != null && valori.length > 0) {
      referralObjectId = valori[0];
    }
  }

  UrlFileStorage ufs = new UrlFileStorage(name);
  ufs.separator = designerField.separator;
  ufs.label = "";
  ufs.initialValue = designerField.initialValue;
  ufs.downloadOnly = designerField.readOnly;
  //ufs.preserveOldValue=designerField.preserveOldValue;

  if (JSP.ex(designerField.urlFileStorage_urlToInclude))
      ufs.urlToInclude = designerField.urlFileStorage_urlToInclude;
  ufs.referralObjectId = docContent == null ? PersistenceHome.NEW_EMPTY_ID : referralObjectId;
  ufs.toHtml(pageContext);

// -----------------  LOOKUP  ------------------------
} else if (classes.contains(LookupSupport.class)) {
  if(designerField.exportable) {
    if (JSP.ex(designerField.label)){
      %><label><%=designerField.label%></label><%
    }
    %><%=separator%><%
    String value = pageState.getEntry(designerField.name).stringValue();
    if(JSP.ex(value)) {
      LookupSupport filter = (LookupSupport)PersistenceHome.findByPrimaryKey(type,value);
      %><%=filter.getDescription()%><%
    }
  } else {
    String hql = "select p.id, p.description from " + type.getName() + " as p";
    String whereForFiltering = "where p.description like :" + SmartCombo.FILTER_PARAM_NAME + " order by p.description";
    String whereForId = "where p.id = :" + SmartCombo.FILTER_PARAM_NAME;
    SmartCombo filter = new SmartCombo(name, hql, whereForFiltering, whereForId);
    filter.label = designerField.label;
    filter.innerLabel = designerField.innerLabel;
    filter.separator = separator;
    filter.classic = designerField.classic;
    if(designerField.maxLength > 0)
      filter.fieldSize = designerField.maxLength;
    filter.readOnly = designerField.readOnly;
    filter.disabled = designerField.readOnly;
    filter.required = designerField.required;
    filter.script = designerField.script;
    filter.preserveOldValue=designerField.preserveOldValue;
    filter.firstEmpty=true;
    filter.onValueSelectedScript=designerField.additionalScript;
    filter.toHtml(pageContext);
  }

// -----------------  IDENTIFIABLE  ------------------------
} else if (classes.contains(Identifiable.class)) {
  if(designerField.exportable) {
    if (JSP.ex(designerField.label)){
      %><label><%=designerField.label%></label><%
    }
    %><%=separator%><%
    String value = pageState.getEntry(designerField.name).stringValue();
    if(JSP.ex(value)) {
      Identifiable identifiable = PersistenceHome.findByPrimaryKey(type,value);
      %><%=identifiable.getName()%><%
    }
  } else {
    String hql = "select p.id, p.name from " + type.getName() + " as p";
    String whereForFiltering = "where p.name like :" + SmartCombo.FILTER_PARAM_NAME + " order by p.name";
    String whereForId = "where p.id = :" + SmartCombo.FILTER_PARAM_NAME;
    SmartCombo filter = new SmartCombo(name, hql, whereForFiltering, whereForId);
    filter.label = designerField.label;
    filter.innerLabel = designerField.innerLabel;
    filter.separator = separator;
    filter.classic = designerField.classic;
    filter.readOnly = designerField.readOnly;
    filter.disabled = designerField.readOnly;
    filter.required = designerField.required;
    filter.preserveOldValue=designerField.preserveOldValue;
    filter.onValueSelectedScript=designerField.additionalScript;

    if(designerField.maxLength > 0)
      filter.fieldSize = designerField.maxLength;
    filter.script = designerField.script;

    filter.toHtml(pageContext);
  }

// -----------------  PERSISTENT FILE  ------------------------
} else if (classes.contains(PersistentFile.class)) {
  if(designerField.exportable) {
    if (JSP.ex(designerField.label)){
      %><label><%=designerField.label%></label><%
    }
    %><%=separator%><%
    String value = pageState.getEntry(designerField.name).stringValue();
    if(JSP.ex(value)) {
      PersistentFile pf =PersistentFile.deserialize(value);
      if(pf != null) {
        %><%=pf.getName()%><%
      }
    }
  } else {
    if(designerField.readOnly) {
      %><%=designerField.label%><%=separator%><%
      String value = pageState.getEntry(designerField.name).stringValue();
      if(value != null && !"".equals(value)) {
        PersistentFile pf =PersistentFile.deserialize(value);
        if(pf != null) {
          MultimediaFile mf = new MultimediaFile(pf,request);
          mf.script = designerField.script;
          mf.toHtml(pageContext);
        }
      }
    } else {
      Uploader upl = new Uploader(name, pageState);
      upl.separator = separator;
      upl.label = designerField.label;
      upl.size = designerField.maxLength;
      upl.required = designerField.required;
      //upl.preserveOldValue=designerField.preserveOldValue;

      upl.toHtml(pageContext);
    }
  }

// -----------------  BOOLEAN  ------------------------
} else if (type.equals(Boolean.class) || "boolean".equals(type.toString())) {
  if(designerField.exportable) {
    if (JSP.ex(designerField.label)){
      %><label><%=designerField.label%></label><%
    }
    %><%=separator%><%
    String value = pageState.getEntry(designerField.name).stringValue();
    if(JSP.ex(value)) {
      if (designerField.displayAsCombo) {
        if(Fields.TRUE.equals(value)) {
          %><%=I18n.get("TRUE")%><%
        } else if(Fields.FALSE.equals(value)) {
          %><%=I18n.get("FALSE")%><%
        }
      } else {
        if(Fields.TRUE.equals(value)) {
          %><span class="teamworkIcon">;</span><%
        } else if(Fields.FALSE.equals(value)) {
          %><span class="teamworkIcon">í</span><%
        }
      }
    }
  } else {
    if (designerField.displayAsCombo || designerField.usedForSearch) {
      CodeValueList cvl = new CodeValueList();
      cvl.addChoose(pageState);
      if (designerField.usedComboForSearch)
        cvl.add("ALL", I18n.get("ALL_MASCULINE"));
      if (designerField.useEmptyForAll)
        cvl.add("ALL", "");
      cvl.add(Fields.TRUE, I18n.get("TRUE"));
      cvl.add(Fields.FALSE, I18n.get("FALSE"));
      Combo boolC = new Combo(name, separator, "", 10, null, cvl, "");
      boolC.disabled =  designerField.readOnly;
      boolC.required = designerField.required;
      boolC.label = designerField.label;
      boolC.innerLabel = designerField.innerLabel;
      boolC.preserveOldValue=designerField.preserveOldValue;
      boolC.script = designerField.script;
      boolC.toHtml(pageContext);
    } else {
      CheckField cb = new CheckField(name, separator,designerField.putLabelFirst);
      cb.label = designerField.label;
      cb.disabled =  designerField.readOnly;
      cb.script = designerField.script;
      cb.additionalOnclickScript= designerField.additionalScript;
      cb.preserveOldValue=designerField.preserveOldValue;
      cb.toHtml(pageContext);
    }
  }

// -----------------  CODE VALUE  ------------------------
} else if (type.equals(CodeValue.class)) {
  if(designerField.exportable) {
    if (JSP.ex(designerField.label)){
      %><label><%=designerField.label%></label><%
    }
    %><%=separator%><%
    String value = pageState.getEntry(designerField.name).stringValue();
    if(JSP.ex(value)) {
      if(designerField.cvl != null && designerField.cvl.keySet().contains(value)) {
        %><%=designerField.cvl.get(value)%><%
      }
    }
  } else {
    if(designerField.displayAsCombo) {
      CodeValueList cvl = new CodeValueList();
      cvl.addAll(designerField.cvl);
      if (!designerField.required )
        cvl.addChoose(pageState);
      Combo boolC = new Combo(name, separator, "", designerField.fieldSize, null, cvl, "");
      boolC.readOnly = designerField.readOnly;
      boolC.required = designerField.required;
      boolC.label = designerField.label;
      boolC.innerLabel = designerField.innerLabel;
      boolC.script = designerField.script;
      boolC.preserveOldValue=designerField.preserveOldValue;
      boolC.setJsOnChange=designerField.additionalScript;
      boolC.toHtml(pageContext);
    } else {
      if(designerField.cvl != null && designerField.cvl.size()>0) {
        %><label><%=designerField.label+(designerField.label != null && !"".equals(designerField.label) && designerField.required ? "*":"")%></label><%=separator%>
        <table class="table"><tr><%
        for(CodeValue cv : designerField.cvl.getList()) {
          %> <td><%
          RadioButton rb = new RadioButton(cv.value,name,cv.code,"","",false,"");
          rb.disabled = designerField.readOnly;
          rb.required = designerField.required;
          rb.script = designerField.script;
          rb.preserveOldValue=designerField.preserveOldValue;

          rb.toHtml(pageContext);
          %></td> <%
        }
        %></tr></table><%
      }
    }
  }
} else {
  %>Unhandled field: <%=designerField.label%>  <%=separator%> type: <%=type.getName()%><%
}

%>