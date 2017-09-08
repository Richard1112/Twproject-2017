<%@ page contentType="application/json; charset=utf-8" pageEncoding="UTF-8" %>
<%@ page import="net.sf.json.JSONObject,
                 org.jblooming.operator.Operator,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.JSONHelper,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.state.PersistentSearch,
                 org.jblooming.waf.view.PageState" %>
<%

  JSONHelper jsonHelper = new JSONHelper();


  PageState pageState = jsonHelper.pageState;
  JSONObject json = jsonHelper.json;
  try {
    Operator logged = pageState.getLoggedOperator();

    //---------------------------- SAVE PERSISTENT SEARCH - LOAD SAVE FILTER ----------------------------------------------------
    if ("SVFILTER".equals(pageState.command)) {
      if (logged != null) {
        String category = pageState.getEntry("cat").stringValueNullIfEmpty();

        if (JSP.ex(logged.getFilter(category + pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty())))
          json.element("filterUpdated",true);

        PersistentSearch.saveSearch(category, pageState);
      }

      //---------------------------- REMOVE PERSISTENT SEARCH FILETR - LOAD SAVE FILTER ----------------------------------------------------
    } else if ("RMFILTER".equals(pageState.command)) {
      if (logged != null) {
        String filterCategory = pageState.getEntry("cat").stringValueNullIfEmpty();
        String filterName = JSP.w(pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty());

        String filterCategoryName = JSP.w(filterCategory) + filterName;
        logged.getFilters().remove(filterCategoryName);
        logged.store();
      }

    }


  } catch (Throwable t) {
    jsonHelper.error(t);
  }

  jsonHelper.close(pageContext);
%>