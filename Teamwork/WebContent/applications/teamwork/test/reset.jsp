<%@ page import="org.jblooming.operator.Operator, org.jblooming.oql.OqlQuery" %><%

  String hql="update "+ Operator.class.getName()+" set enabled=true, password='' ";
  int i = new OqlQuery(hql).getQuery().executeUpdate();
%><h1><%=i%> operator re-enabled and password reset</h1>

