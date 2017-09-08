<%@ page import="com.twproject.resource.Resource, com.twproject.task.Assignment, com.twproject.task.Task, com.twproject.task.businessLogic.ExpenseController, com.twproject.task.financial.Cost, com.twproject.waf.TeamworkPopUpScreen,
                 com.twproject.worklog.businessLogic.WorklogBricks, org.jblooming.ontology.IdentifiableSupport, org.jblooming.operator.Operator, org.jblooming.system.SystemConstants, org.jblooming.utilities.DateUtilities,
                 org.jblooming.utilities.JSP, org.jblooming.utilities.file.FileUtilities, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.display.Img, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.RadioButton, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.settings.I18n,org.jblooming.waf.view.PageSeed,org.jblooming.waf.view.PageState,java.util.Collections,java.util.Comparator,
                 java.util.List"%>
<%!
  public class CompByTask implements Comparator<Object[]> {
    public int compare(Object[] t1, Object[] t2) {
      Task task2 = ((Assignment)t2[0]).getTask();
      Task task1 = ((Assignment)t1[0]).getTask();
      if (task1.equals(task2)) {
        Resource resource2 = ((Assignment)t2[0]).getResource();
        Resource resource1 = ((Assignment)t1[0]).getResource();
        if (!resource2.equals(resource1))
           return resource1.getName().compareToIgnoreCase(resource2.getName());
        else
          return ((Cost)t1[1]).getCreationDate().compareTo(((Cost)t2[1]).getCreationDate());
      } else
        return (task1.getCode()+task1.getName()).compareToIgnoreCase(task2.getCode()+task2.getName());
     }
  }
  public class CompByRes implements Comparator<Object[]> {

    public int compare(Object[] t1, Object[] t2) {
      Resource res1 = ((Assignment)t2[0]).getResource();
      Resource res2 = ((Assignment)t1[0]).getResource();

      if (res2.equals(res1))
        return ((Cost)t1[1]).getCreationDate().compareTo(((Cost)t2[1]).getCreationDate());
      else
        return (res2.getName()).compareTo(res1.getName());
     }
  }
%>
<%

    PageState pageState = PageState.getCurrentPageState(request);

    if (!pageState.screenRunning) {
      pageState.screenRunning = true;

      final ScreenArea body = new ScreenArea(new ExpenseController(), request);
      TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
      lw.register(pageState);
      pageState.perform(request, response).toHtml(pageContext);
    } else {


    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.FIND);
    self.addClientEntries(pageState.getClientEntries());
    self.removeEntriesMatching("PRINT_");

    Form f = new Form(self);
    f.alertOnChange = false;
    f.id="printCost";
    pageState.setForm(f);
    f.start(pageContext);

    Operator op = pageState.getLoggedOperator();


    boolean ordByTask = "TASK".equals(pageState.getEntryOrDefault("PRINT_WORKLOG_ORDER","TASK").stringValue());
    boolean ordByResource = "RES".equals(pageState.getEntryOrDefault("PRINT_WORKLOG_ORDER","RES").stringValue());

    boolean showAttachment= pageState.getEntryOrDefault("PRINT_COST_SHOW_ATTACH").checkFieldValue();

    List<Object[]> ass_costs = pageState.getPage().getAllElements();

    if (ordByTask) {
      CompByTask c = new CompByTask();
      Collections.sort(ass_costs, c);
    } else {
      CompByRes c = new CompByRes();
      Collections.sort(ass_costs, c);

    }

    Img logo = new Img(ApplicationState.getApplicationSetting(SystemConstants.PRINT_LOGO), "");
    %>

<style>
  .costAttachment img{
    max-width: 800px;
    max-height: 800px;
    margin-bottom: 100px;
  }
  .totalsLabel {
    opacity: .6;
    margin-right: 30px;;
  }
</style>

<table border="0" width="100%" align="center" cellpadding="5" cellspacing="0" class="noprint">
    <tr>
      <td align="left" width="90%"><%logo.toHtml(pageContext);%></td>
      <td align="right"><%

        ButtonJS print = new ButtonJS("window.print();");
        print.label = "";
        print.toolTip = I18n.get("PRINT_PAGE");
        print.iconChar = "p";
        print.toHtml(pageContext);

      %></td>
    </tr>
</table>

<div id="printFilter" style="visibility:visible;" class="noprint"> <%
  //filters

    Container filter = new Container("IP");
    filter.level = 2;
    filter.start(pageContext);



  {
      RadioButton showCh = new RadioButton(I18n.get("WORKLOG_ORDER_BY_TASK"),"PRINT_WORKLOG_ORDER", "TASK","", null,false,null);
      showCh.label= I18n.get("WORKLOG_ORDER_BY_TASK");
      showCh.script="$('#printCost').submit();";
      showCh.toHtmlI18n(pageContext);
    }

    {
      RadioButton showCh = new RadioButton(I18n.get("WORKLOG_ORDER_BY_RESOURCE"),"PRINT_WORKLOG_ORDER", "RES","", null,false,null);
      showCh.label= I18n.get("WORKLOG_ORDER_BY_RESOURCE");
      showCh.script="$('#printCost').submit();";
      showCh.toHtmlI18n(pageContext);
    }


    {
      CheckField showCh = new CheckField("PRINT_COST_SHOW_ATTACH", "", false);
      showCh.label= I18n.get("SHOW_ATTACHMENT");
      showCh.additionalOnclickScript="$('#printCost').submit();";
      showCh.toHtmlI18n(pageContext);
    }

    filter.end(pageContext);

  %></div> <%




/*________________________________________________________________________________________________________________________________________________________________________


  results

________________________________________________________________________________________________________________________________________________________________________

*/

    %><table border="0" width="100%" align="center" cellpadding="5" cellspacing="0" style="font-size: 70%;" class="table"><tr>
      <th class="tableHead"><%="id"%></th>

      <th class="tableHead"><%=I18n.get("STATUS")%></th>
      <%if ((ordByTask)){%>
        <th class="tableHead"><%=I18n.get("RESOURCE")%></th>
        <th class="tableHead"><%=I18n.get("ROLE")%></th>
      <%}else{%>
        <th class="tableHead"><%=I18n.get("TASK")%></th>
        <th class="tableHead"><%=I18n.get("CODE")%></th>
      <%}%>

      <th class="tableHead"><%=I18n.get("COST_CLASSIFICATION")%></th>
      <th class="tableHead"><%=I18n.get("WORKLOG_DATE")%></th>
      <th class="tableHead"><%=I18n.get("DESCRIPTION")%></th>
      <th class="tableHead"><%=I18n.get("COST_DONE")%></th>
      </tr><%

      double granTotal=0;
      double subTotal=0;


      if (ass_costs != null && ass_costs.size() > 0) {

        IdentifiableSupport curreElment=null;

        for (Object[] obs: ass_costs) {
          Assignment assig= (Assignment) obs[0];
          Cost cost=(Cost)obs[1];


/*
________________________________________________________________________________________________________________________________________________________________________


task header/footer

________________________________________________________________________________________________________________________________________________________________________

*/
              if (curreElment ==null || !curreElment.equals(ordByTask?assig.getTask():assig.getResource())) {
                //footer
                if (curreElment !=null) {
                  %><tr class="totals">
                  <td align="right" colspan="7"><span class="textSmall totalsLabel"><%=curreElment.getDisplayName()%></span> <%=I18n.get("PARTIAL_TOTAL")%></td>
                  <td align="right" ><%=JSP.currency(subTotal)%></td>
                  </tr> <%
                  subTotal=0;
                }
                //header
                curreElment = ordByTask?assig.getTask():assig.getResource();
                 %><tr><td colspan="99" align="left"  class="tableSection">
                   <div class="childNode"><h2><%=curreElment.getDisplayName()%></h2></div>
                 </td></tr><%
              }
/*
________________________________________________________________________________________________________________________________________________________________________


cost line

________________________________________________________________________________________________________________________________________________________________________

*/
            subTotal = subTotal+ cost.getRealCost();
            granTotal = granTotal+ cost.getRealCost();


            %><tr><%

            %><td align="center"><small><%=cost.getId()%></small></td><%
            %><td align="center"><%=WorklogBricks.drawStatus(cost.getStatus(), pageState)%></td><%
            if (ordByTask) {
              %><td><%=assig.getResource().getDisplayName()%></td><%
              %><td><%=assig.getRole().getCode()%></td><%
            } else {
              %><td>
                <%if (I18n.isActive("CUSTOM_FEATURE_ALWAYS_SHOW_TASK_PATH")){
                %><div class="pathSmall"><%=assig.getTask().getPath(" / ", false)%></div><%
                }%>
                <%=assig.getTask().getName()%>
              </td><%
              %><td><%=assig.getTask().getCode()%></td><%
}
            %><td nowrap><%=cost.getClassification()==null?"&nbsp;":cost.getClassification().getDisplayName()%></td> <%
            %><td nowrap><%=DateUtilities.dateToFullString(cost.getCreationDate())%></td> <%
            %><td nowrap><%=JSP.w(cost.getDescription())%></td> <%
            %><td nowrap align="right"><%=JSP.currency(cost.getRealCost())%></td> <%
            if(showAttachment){
              %></tr><%
              if (cost.getAttachment()!=null &&FileUtilities.isImageByFileExt(cost.getAttachment().getFileExtension()) ) {
                  %><tr class="pageBreak"><td colspan="99" class="costAttachment"><%
                    new Img(cost.getAttachment(),cost.getAttachment().getOriginalFileName()).toHtml(pageContext);
                  %></td></tr><%
              }
            }
          }

/*
________________________________________________________________________________________________________________________________________________________________________


last task footer

________________________________________________________________________________________________________________________________________________________________________

*/
          if (curreElment !=null) {
            %><tr class="totals">
            <td align="right" colspan="7"><span class="textSmall totalsLabel"><%=curreElment.getDisplayName()%></span> <%=I18n.get("PARTIAL_TOTAL")%></td>
            <td align="right" ><%=JSP.currency(subTotal)%></td>
            </tr> <%
          }

/*
________________________________________________________________________________________________________________________________________________________________________


totals

________________________________________________________________________________________________________________________________________________________________________

*/
        %>

      <tr  class="totals">
        <td align="right" colspan="7"><%=I18n.get("TOTAL")%></td>
        <td align="right"><%=JSP.currency(granTotal)%></td>
      </tr>
      <%

      } else {
      %> <tr><td colspan="99"><%=I18n.get("NO_COSTS")%></td></tr><%
      }
    %></table><%

      f.end(pageContext);
  }
%>