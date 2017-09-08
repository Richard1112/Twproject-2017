<%@ page contentType="application/json; charset=utf-8" pageEncoding="UTF-8" %>
<%@ page import="com.twproject.mobile.MobileAjaxController,
                 org.jblooming.waf.JSONHelper" %><%

  JSONHelper jsonHelper = new JSONHelper();
  jsonHelper.json=new MobileAjaxController().perform(request,response);
  jsonHelper.close(pageContext);
%>