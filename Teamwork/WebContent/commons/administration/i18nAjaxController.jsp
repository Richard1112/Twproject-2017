<%@ page import="net.sf.json.JSONObject, org.jblooming.operator.Operator, org.jblooming.oql.OqlQuery, org.jblooming.security.PlatformPermissions,
org.jblooming.utilities.JSP, org.jblooming.waf.JSONHelper, org.jblooming.waf.constants.Commands, org.jblooming.waf.settings.ApplicationState,
org.jblooming.waf.settings.I18n, org.jblooming.waf.settings.I18nEntryPersistent, org.jblooming.waf.settings.businessLogic.I18nAction, org.jblooming.waf.view.PageState" %><%

PageState pageState = PageState.getCurrentPageState(request);

  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;
  try {

  Operator operator = pageState.getLoggedOperator();
  if (Commands.SAVE.equals(pageState.command)) {
    operator.testIsAdministrator();
    String code = pageState.getEntry("code").stringValueNullIfEmpty();
    String lang = pageState.getEntry("lang").stringValueNullIfEmpty();
    String appl = pageState.getEntry("appl").stringValueNullIfEmpty();
    String label = pageState.getEntry("label").stringValueNullIfEmpty();
    if (JSP.ex(code, lang, appl)) {
      I18nAction i18Act = new I18nAction();
      i18Act.saveEntryInMemory(code, appl, lang, label);

      if (!ApplicationState.platformConfiguration.development) {
        // remove all code
        OqlQuery oql = new OqlQuery("delete from " + I18nEntryPersistent.class.getName() + " where code=:code and language=:lang and application=:appl");
        oql.getQuery().setString("code", code);
        oql.getQuery().setString("lang", lang);
        oql.getQuery().setString("appl", appl);
        oql.getQuery().executeUpdate();


        if (label != null) {
          I18nEntryPersistent ent = new I18nEntryPersistent();
          ent.setCode(code);
          ent.setApplication(appl);
          ent.setLanguage(lang);
          ent.setValue(label);
          ent.store();
        } else {
          I18n.removeEntry(code, appl, lang);
        }
      }
      json.element("mustDump",I18n.dumpNeeded);
    }


  } else   if ("SVSMPL".equals(pageState.command)) {
      String code = pageState.getEntry("code").stringValueNullIfEmpty();
      String lang = pageState.getEntry("lang").stringValueNullIfEmpty();
      String appl = pageState.getEntry("appl").stringValueNullIfEmpty();
      String label = pageState.getEntry("label").stringValueNullIfEmpty();

      if (JSP.ex(code, lang, appl)) {
        //test security
        if (operator.hasPermissionAsAdmin() || operator.hasPermissionFor(PlatformPermissions.i18n_manage) || lang.equalsIgnoreCase(operator.getOption("CAN_EDIT_THIS_LANGUAGE"))){
          I18nAction i18Act = new I18nAction();
          i18Act.saveEntryInMemory(code, appl, lang, label);

          // from save simple always save on file
          i18Act.cmdDump(pageState, request);
        }
      }
      
    } else   if ("ASSLANG".equals(pageState.command)) {
      operator.testIsAdministrator();

      Operator op= Operator.load(pageState.getEntry("userId").intValueNoErrorCodeNoExc());
      if (op!=null){
        String lang=pageState.getEntry("lang").stringValueNullIfEmpty();
        if (JSP.ex(lang))
          op.getOptions().put("CAN_EDIT_THIS_LANGUAGE",lang.toUpperCase());
        else
          op.getOptions().remove("CAN_EDIT_THIS_LANGUAGE");

        op.store();
      }


  } else if ("RENAME".equals(pageState.command)) {
    if (operator.hasPermissionAsAdmin() || operator.hasPermissionFor(PlatformPermissions.i18n_manage)) {
      String oldCode = pageState.getEntry("oldcode").stringValueNullIfEmpty();
      String newCode = pageState.getEntry("newcode").stringValueNullIfEmpty();
      String appl = pageState.getEntry("appl").stringValueNullIfEmpty();
      if (JSP.ex(oldCode, newCode, appl)) {
        I18n.I18nEntry nEntry = I18n.codeEntries.get(oldCode);
        nEntry.code = newCode;
        I18n.codeEntries.remove(oldCode);
        I18n.codeEntries.put(newCode, nEntry);
        I18n.dumpNeeded = true;
        json.element("mustDump", I18n.dumpNeeded);
      }
    }

  } else if ("CMD_REMOVE_LABEL_ALL".equals(pageState.command)) {
    I18n i18nManager = ApplicationState.i18n;
    if (pageState.mainObjectId!=null)
      i18nManager.codeEntries.remove(pageState.mainObjectId);


  } else if (I18n.CMD_REMOVE_LABEL.equals(pageState.command)) {
      I18nAction a = new I18nAction();
      a.cmdRemoveLabel(pageState);
    }
  } catch (Throwable t) {
    jsonHelper.error(t);
  }

  jsonHelper.close(pageContext);


%>