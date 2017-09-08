<%@ page import="com.twproject.resource.Resource, com.twproject.task.Assignment, com.twproject.task.Task, com.twproject.task.businessLogic.ExpenseController, com.twproject.task.financial.Cost, com.twproject.waf.TeamworkPopUpScreen,
                 com.twproject.worklog.businessLogic.WorklogBricks, org.jblooming.ontology.IdentifiableSupport, org.jblooming.operator.Operator, org.jblooming.system.SystemConstants, org.jblooming.utilities.DateUtilities,
                 org.jblooming.utilities.JSP, org.jblooming.utilities.file.FileUtilities, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.display.Img, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.RadioButton, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.settings.I18n,org.jblooming.waf.view.PageSeed,org.jblooming.waf.view.PageState,java.util.Collections,java.util.Comparator,
                 java.util.List, com.twproject.task.businessLogic.AdditionalCostController"%>
<%!
  public class CompByTask implements Comparator<Object[]> {
    public int compare(Object[] t1, Object[] t2) {
      Task task2 = ((Task)t2[0]);
      Task task1 = ((Task)t1[0]);
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

      final ScreenArea body = new ScreenArea(new AdditionalCostController(), request);
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


      boolean showAttachment= pageState.getEntryOrDefault("PRINT_COST_SHOW_ATTACH").checkFieldValue();

      Operator op = pageState.getLoggedOperator();

    List<Object[]> task_costs = pageState.getPage().getAllElements();

    CompByTask c = new CompByTask();
    Collections.sort(task_costs, c);

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
      <th class="tableHead"><%=I18n.get("TASK")%></th>
      <th class="tableHead"><%=I18n.get("CODE")%></th>

      <th class="tableHead"><%=I18n.get("COST_CLASSIFICATION")%></th>
      <th class="tableHead"><%=I18n.get("WORKLOG_DATE")%></th>
      <th class="tableHead"><%=I18n.get("DESCRIPTION")%></th>
      <th class="tableHead"><%=I18n.get("COST_ESTIMATED")%></th>
      <th class="tableHead"><%=I18n.get("COST_DONE")%></th>
      </tr><%

      double granTotal=0;
      double granTotalEstim=0;
      double subTotal=0;
      double subTotalEstim=0;


      if (task_costs != null && task_costs.size() > 0) {

        IdentifiableSupport curreElment=null;

        for (Object[] obs: task_costs) {
          Task task= (Task) obs[0];
          Cost cost=(Cost)obs[1];


/*
________________________________________________________________________________________________________________________________________________________________________


task header/footer

________________________________________________________________________________________________________________________________________________________________________

*/
              if (curreElment ==null || !curreElment.equals(task)) {
                //footer
                if (curreElment !=null) {
                  %><tr class="totals">
                  <td align="right" colspan="6"><span class="textSmall totalsLabel"><%=curreElment.getDisplayName()%></span> <%=I18n.get("PARTIAL_TOTAL")%></td>
                  <td align="right" ><%=JSP.currency(subTotalEstim)%></td>
                  <td align="right" ><%=JSP.currency(subTotal)%></td>
                  </tr> <%
                  subTotalEstim=0;
                  subTotal=0;
                }
                //header
                curreElment = task;
                 %><tr><td colspan="99" align="left"  class="tableSection">
                   <div class="childNode"><h2><%=curreElment.getDisplayName()%></h2></div>
                 </td></tr><%
              }
/*
________________________________________________________________________________________________________________________________________________________________________


cost line

________________________________________________________________________________________________________________________________________________________________________

*/
            subTotalEstim = subTotalEstim+ cost.getEstimatedCost();
            subTotal = subTotal+ cost.getRealCost();
            granTotalEstim = granTotalEstim+ cost.getEstimatedCost();
            granTotal = granTotal+ cost.getRealCost();


            %><tr><%

            %><td align="center"><small><%=cost.getId()%></small></td>
              <td>
                <%if (I18n.isActive("CUSTOM_FEATURE_ALWAYS_SHOW_TASK_PATH")){
                %><div class="pathSmall"><%=task.getPath(" / ", false)%></div><%
                }%>
                <%=task.getName()%>
              </td>
              <td><%=task.getCode()%></td>

            <td nowrap><%=cost.getClassification()==null?"&nbsp;":cost.getClassification().getDisplayName()%></td> <%
            %><td nowrap><%=DateUtilities.dateToString(cost.getCreationDate())%></td> <%
            %><td nowrap><%=JSP.w(cost.getDescription())%></td> <%
            %><td nowrap align="right"><%=JSP.currency(cost.getEstimatedCost())%></td> <%
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
            <td align="right" colspan="6"><span class="textSmall totalsLabel"><%=curreElment.getDisplayName()%></span> <%=I18n.get("PARTIAL_TOTAL")%></td>
            <td align="right" ><%=JSP.currency(subTotalEstim)%></td>
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
        <td align="right" colspan="6"><%=I18n.get("TOTAL")%></td>
        <td align="right"><%=JSP.currency(granTotalEstim)%></td>
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