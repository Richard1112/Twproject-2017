<%@ page import="org.hibernate.Hibernate,
                 org.jblooming.ontology.*,
                 org.jblooming.persistence.hibernate.HibernateUtilities,
                 org.jblooming.persistence.objectEditor.FieldDrawer,
                 org.jblooming.persistence.objectEditor.FieldFeature,
                 org.jblooming.utilities.CodeValueList,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.ReflectionUtilities,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.input.*,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageState,
                 java.io.Serializable,
                 java.lang.reflect.Field,
                 java.util.Date, java.util.List" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  FieldDrawer fd = (FieldDrawer) JspIncluderSupport.getCurrentInstance(request);
  FieldFeature fieldFeature = fd.fieldFeature;
  String separator = (fieldFeature.separator == null) ? "<br>" : fieldFeature.separator;
  int size = fieldFeature.fieldSize;
%><td align="left" valign="top" nowrap><%
  if (fieldFeature.blank != null) {
%><%=fieldFeature.blank%><%=separator%><%=fieldFeature.blank%><%

} else {
  Field field = ReflectionUtilities.getField(fieldFeature.propertyName, fd.mainObjectClass);
  field.setAccessible(true);
  Class type = field.getType();
  List classes = ReflectionUtilities.getInheritedClasses(type);

  if (fieldFeature.smartCombo != null) {

    fieldFeature.smartCombo.label = fieldFeature.label;
    fieldFeature.smartCombo.separator = separator;
    if (size != -1)
      fieldFeature.smartCombo.fieldSize = size;
    fieldFeature.smartCombo.toHtml(pageContext);

  } else if (fieldFeature.comboElement != null) {
    fieldFeature.comboElement.separator = separator;
    fieldFeature.comboElement.disabled = fieldFeature.readOnly;
    fieldFeature.comboElement.required = fieldFeature.required;
    fieldFeature.comboElement.label = fieldFeature.label;
    if (size != -1)
      fieldFeature.comboElement.maxLenght = size;
    fieldFeature.comboElement.toHtml(pageContext);

  } else if (fieldFeature.pageSeed != null) {
    fieldFeature.pageSeed.addClientEntry("PARENT_ID", pageState.getMainObject().getId());
    fieldFeature.pageSeed.addClientEntry("PARENT_CLASS", Hibernate.getClass(pageState.getMainObject()).getName());
    fieldFeature.pageSeed.addClientEntry("PARENT_PROPERTY", fieldFeature.propertyName);
    fieldFeature.pageSeed.addClientEntry("PARENT_URL", request.getContextPath() + "/" + pageState.href);
    ButtonLink bl = ButtonLink.getTextualInstance(fieldFeature.fieldName, fieldFeature.pageSeed);
    Object value = ReflectionUtilities.getFieldValue(fieldFeature.propertyName, pageState.getMainObject());

    //if value is not null, activate link with edit
    if (value != null) {
      fieldFeature.pageSeed.setCommand(Commands.EDIT);
      fieldFeature.pageSeed.setMainObjectId(((Identifiable) value).getId());

      //otherwise with add
    } else {
      fieldFeature.pageSeed.setCommand(Commands.ADD);
    }
    %><%=I18n.get("LINK_TO")%>:<%=separator%><%bl.toHtml(pageContext);%><%

} else {

  if (type.equals(String.class) || type.equals(Serializable.class)) {

    int columnLength = 0;

    // specified by hand?
    if (size > -1) {
      columnLength = size;
    }

    // not set and is persistent? ask hibernate
    if (columnLength == 0 && ReflectionUtilities.getInheritedClasses(fd.mainObjectClass).contains(Identifiable.class))
      columnLength = HibernateUtilities.getColumnLength(fd.mainObjectClass, fieldFeature.propertyName);

    // not set and auto by value CE size
    if (columnLength == 0 && fd.autoSize) {
       columnLength = Math.max(JSP.w(pageState.getEntry(fieldFeature.fieldName).stringValueNullIfEmpty()).length(), 5);
    }


    if (columnLength > 60) {
      TextArea ta = new TextArea(fieldFeature.label, fieldFeature.fieldName, separator, 40, 2, "", false, false, "");
      ta.readOnly = fieldFeature.readOnly;
      ta.required = fieldFeature.required;

      if (fd.submitOnKeyReturn)
        ta.addKeyPressControl(13, "obj('" + fd.form.getUniqueName() + "').submit();", "onkeyup");
      ta.toHtml(pageContext);
    } else {
      TextField tf = new TextField(fieldFeature.label, fieldFeature.fieldName, separator, columnLength, fieldFeature.readOnly);
      if (fd.form != null && fd.submitOnKeyReturn)
        tf.addKeyPressControl(13, "obj('" + fd.form.getUniqueName() + "').submit();", "onkeyup");

      tf.toHtml(pageContext);
    }

  } else if (classes.contains(Date.class)) {
    DateField datefield = new DateField(fieldFeature.fieldName);
    datefield.setSearchField(fieldFeature.usedForSearch);
    datefield.labelstr = fieldFeature.label;
    datefield.separator = separator;
    datefield.readOnly = fieldFeature.readOnly;
    datefield.required = fieldFeature.required;
    if (fd.submitOnKeyReturn)
      datefield.addKeyPressControl(13, "obj('" + fd.form.getUniqueName() + "').submit();", "onkeyup");
    datefield.toHtml(pageContext);

  } else if (classes.contains(Integer.class) || classes.contains(int.class)) {
    int columnLength = 20;
    if (size != -1)
      columnLength = size;
    if (classes.contains(int.class)) columnLength = 4;

    TextField tf = TextField.getIntegerInstance(fieldFeature.fieldName);
    tf.separator = separator;
    tf.label = fieldFeature.label;
    tf.fieldSize = columnLength;
    if (fd.submitOnKeyReturn)
      tf.addKeyPressControl(13, "obj('" + fd.form.getUniqueName() + "').submit();", "onkeyup");
    tf.toHtml(pageContext);

  } else if (classes.contains(Float.class) || classes.contains(float.class) || classes.contains(Double.class) || classes.contains(double.class)) {
    int columnLength = 10;
    if (size != -1)
      columnLength = size;
    if (classes.contains(float.class)) columnLength = 4;

    TextField tf = TextField.getDoubleInstance(fieldFeature.fieldName);
    tf.separator = separator;
    tf.label = fieldFeature.label;
    tf.fieldSize = columnLength;
    if (fd.submitOnKeyReturn)
      tf.addKeyPressControl(13, "obj('" + fd.form.getUniqueName() + "').submit();", "onkeyup");
    tf.toHtml(pageContext);

  } else if (classes.contains(LookupSupport.class)) {
    String hql = "select p.id, p.description from " + type.getName() + " as p";
    String whereForFiltering = "where upper(p.description) like :" + SmartCombo.FILTER_PARAM_NAME + " order by p.description";
    String whereForId = "where p.id = :" + SmartCombo.FILTER_PARAM_NAME;
    SmartCombo filter = new SmartCombo(fieldFeature.fieldName, hql, whereForFiltering, whereForId);
    filter.label = fieldFeature.label;
    filter.separator = separator;
    filter.convertToUpper = true;
    if (size != -1)
      filter.fieldSize = size;
    filter.readOnly = fieldFeature.readOnly;
    filter.required = fieldFeature.required;

    filter.toHtml(pageContext);

  } else if (classes.contains(PersistentFile.class)) {
    Uploader upl = new Uploader(fieldFeature.fieldName, pageState);
    upl.separator = separator;
    upl.label = fieldFeature.label;
    if (size != -1)
      upl.size = size;
    upl.readOnly = fieldFeature.readOnly;
    upl.disabled = fieldFeature.readOnly;
    upl.required = fieldFeature.required;

    upl.toHtml(pageContext);

  } else if (type.equals(Boolean.class) || "boolean".equals(type.toString())) {

    if (fieldFeature.boolAsCombo) {

      CodeValueList cvl = new CodeValueList();
      if (fieldFeature.usedComboForSearch)
        cvl.add("ALL", I18n.get("ALL_MASCULINE"));
      if (fieldFeature.useEmptyForAll)
        cvl.add("ALL", "");
      cvl.add(Fields.TRUE, I18n.get("TRUE"));
      cvl.add(Fields.FALSE, I18n.get("FALSE"));

      Combo boolC = new Combo(fieldFeature.fieldName, separator, "", 10, null, cvl, "");
      boolC.disabled = fieldFeature.readOnly;
      boolC.required = fieldFeature.required;
      boolC.label = fieldFeature.label;

      boolC.toHtml(pageContext);

    } else {
      CheckField cb = new CheckField(fieldFeature.label, fieldFeature.fieldName, separator,  false);
      cb.disabled = fieldFeature.readOnly;
      cb.label = fieldFeature.label;
      cb.separator = "&nbsp;";
      cb.toHtml(pageContext);
    }
  } else if (type.isEnum()) {
    Object[] obj = type.getEnumConstants();
    CodeValueList cvl = new CodeValueList();
    cvl.add("", "&nbsp;");
    for (int i = 0; i < obj.length; i++) {
      Object og = obj[i];
      cvl.add(og.toString(), og.toString());
    }

    Combo enumC = new Combo(fieldFeature.fieldName, separator, "", 10, null, cvl, "");
    enumC.disabled = fieldFeature.readOnly;
    enumC.required = fieldFeature.required;
    enumC.label = fieldFeature.label;

    enumC.toHtml(pageContext);


  } else if (classes.contains(SerializedMap.class) ||
          classes.contains(SerializedList.class)) {

    TextField tf = new TextField(fieldFeature.label, fieldFeature.fieldName, separator, 30, fieldFeature.readOnly);
    tf.autoSize = fd.autoSize;
    if (size != -1)
      tf.fieldSize = size;
    if (fd.form != null && fd.submitOnKeyReturn)
      tf.addKeyPressControl(13, "obj('" + fd.form.getUniqueName() + "').submit();", "onkeyup");

    tf.toHtml(pageContext);


  } else {
%>Unhandled field: <%=fieldFeature.label%>  <%=separator%>  type: <%=type.getName()%> <%
      }
    }
  }

%></td>