<%@ page import="org.jblooming.oql.OqlQuery,
                 org.jblooming.tracer.Tracer,
                 org.jblooming.utilities.CodeValueList,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.input.Combo,
                 org.jblooming.waf.html.input.SmartCombo,
                 org.jblooming.waf.settings.PersistenceConfiguration,
                 org.jblooming.waf.view.PageState,
                 java.io.Serializable,
                 java.util.List" %>
<%

  SmartCombo smartCombo = (SmartCombo) JspIncluderSupport.getCurrentInstance(request);
  PageState pageState = PageState.getCurrentPageState(request);


  List<Object[]> prs = smartCombo.fillResultList("","");
  CodeValueList cvl = new CodeValueList();
  if(smartCombo.addAllowed)
    cvl.addChoose(pageState);

  //check if selected id is present in the set
  Serializable id = pageState.getEntry(smartCombo.fieldName).stringValueNullIfEmpty();
  if (JSP.ex(id)) {

    boolean found = false;
    for (Object[] value : prs) {

      String code = "" + value[0];
      if ((id + "").equals(code)) {
        found = true;
        break;
      }
    }

    if (!found) {
      // recover id and description from db

      OqlQuery oqlForId;

      oqlForId = new OqlQuery(smartCombo.hql + " " + smartCombo.whereForId);

      // modified R&P for PosgreSql on  Nov 11, 2008:
      if (PersistenceConfiguration.getDefaultPersistenceConfiguration().dialect.equals(org.hibernate.dialect.PostgreSQLDialectDBBlobs.class))
        oqlForId.getQuery().setInteger(SmartCombo.FILTER_PARAM_NAME, Integer.parseInt(id + ""));
      else
        oqlForId.getQuery().setString(SmartCombo.FILTER_PARAM_NAME, id + "");

      Object result = null;

      try {
        result = oqlForId.uniqueResultNullIfEmpty();
      } catch (Throwable e) {
        Tracer.platformLogger.error(e);
      }

      if (result != null) {
        Object o = ((Object[]) result)[smartCombo.columnToCopyInDescription];
        //add to cvl
        cvl.add(id+"",""+o);
      }
    }

  }


  for (Object[] value : prs) {

    String code = "" + value[0];
    String val = "";
    for (int i = 1; i < value.length; i++) {
      val += value[i] + " ";
    }
    cvl.add(code, val);
  }


  Combo combo = new Combo(smartCombo.label, smartCombo.fieldName, smartCombo.separator, null, smartCombo.fieldSize, smartCombo.initialSelectedCode, cvl, smartCombo.script);

  combo.launchedJsOnActionListened = smartCombo.launchedJsOnActionListened;
  combo.actionListened = smartCombo.actionListened;
  combo.readOnly = smartCombo.readOnly;
  combo.disabled = smartCombo.disabled;
  combo.required = smartCombo.required;
  combo.preserveOldValue = smartCombo.preserveOldValue;
  combo.onBlurScript = smartCombo.onBlurAdditionalScript;
  combo.setJsOnChange = smartCombo.onValueSelectedScript;


  combo.toHtml(pageContext);


%>
