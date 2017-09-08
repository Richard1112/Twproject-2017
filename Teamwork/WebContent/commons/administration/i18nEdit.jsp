<%@ page import="org.jblooming.waf.ScreenBasic,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageState" %><%
  PageState pageState = PageState.getCurrentPageState(request);
  pageState.getLoggedOperator().testIsAdministrator();

  if (!pageState.screenRunning) {

    ScreenBasic lw = ScreenBasic.preparePage(null, pageContext);
    if (I18n.EDIT_STATUS_EDIT.equals(I18n.getEditStatus())) {
      lw.menu = null;
    }


    // hack to temporary disable i18nedit
    String old_status = I18n.getEditStatus();
    I18n.setEditStatus(I18n.EDIT_STATUS_READ);
    // temporary disable catching labels
    boolean catchState = I18n.catchUsedLabels;
    I18n.catchUsedLabels = false;

    pageState.perform(request, response).toHtml(pageContext);

    // hack to temporary disable i18nedit
    I18n.setEditStatus(old_status);
    I18n.catchUsedLabels = catchState;
  } else {


%><jsp:include page="partI18nEdit.jsp"/><%

  }
%>