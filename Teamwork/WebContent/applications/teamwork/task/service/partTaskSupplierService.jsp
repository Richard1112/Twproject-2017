<%@page import="org.jblooming.waf.html.core.JST"%>
<%@page import="com.teamwork.expand.TaskServiceBricks"%>
<%@ page import="org.jblooming.designer.DesignerField,
                 org.jblooming.designer.Detail,
                 org.jblooming.ontology.Identifiable,
                 org.jblooming.ontology.LookupSupport,
                 org.jblooming.utilities.ReflectionUtilities,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.input.DateField,
                 org.jblooming.waf.html.input.SmartCombo,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageState,
                 java.util.Date,
                 java.util.List"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  //String detailName = pageState.getEntry("detailName").stringValue();
  String rowLine = pageState.getEntry("rowLine").stringValue();
  SmartCombo supCombo = TaskServiceBricks.getSupplierCombo("NEW_SUPPLIER_"+rowLine, pageState, "SUPPLIER");
  supCombo.fieldSize = 18;
  supCombo.label = "";
  supCombo.required=true;
  //supCombo.linkToEntity=null;
  supCombo.addEntityButton=new ButtonJS(I18n.get("ADD_SUPPLIER"),"createNewResource($(this));");
  supCombo.separator="";
  SmartCombo serviceType = TaskServiceBricks.getServiceTypeCombo("NEW_SERVICE_TYPE_"+rowLine, pageState);
  serviceType.fieldSize = 18;
  serviceType.label="";
 // serviceType.readOnly=true;
  serviceType.separator="";
  serviceType.required=true;
  SmartCombo serviceContent = TaskServiceBricks.getServiceContentCombo("NEW_SERVICE_CONTENT_"+rowLine, pageState);
  serviceContent.fieldSize = 18;
  serviceContent.label="";
  serviceContent.separator="";
  serviceContent.required=true;
 // serviceContent.readOnly=true;
  DateField dfL = new DateField("NEW_SERVICE_DATE_"+rowLine);
  dfL.labelstr="";
  dfL.required=true;
  dfL.separator="";
  

%>

<tr class="assigRow" >
  <td><%supCombo.toHtml(pageContext);%></td>
  <td><%serviceType.toHtml(pageContext);%></td>
  <td><%serviceContent.toHtml(pageContext);%></td>
  <td><input type="text" name="NEW_TOTAL_NUM_<%= rowLine%>" autocomplete="off"  required="true"  size="5" class="formElements validated"  entryType="DURATIONMILLIS"></td>
  <td><%dfL.toHtml(pageContext);%></td>
   <td>&nbsp;&nbsp;</td>
  <td align="center"><span class="teamworkIcon delAssig delete" style="cursor: pointer" onclick="$(this).closest('tr').remove();">d</span></td>
</tr>