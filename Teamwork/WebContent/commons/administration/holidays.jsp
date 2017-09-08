<%@ page import="org.jblooming.agenda.CompanyCalendar, org.jblooming.utilities.DateUtilities, org.jblooming.waf.ScreenBasic, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.core.HtmlIncluder, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Date, org.jblooming.waf.html.button.ButtonLink"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    ScreenBasic.preparePage(pageContext);
    pageState.perform(request, response).toHtml(pageContext);
    pageState.getLoggedOperator().testIsAdministrator();

  } else {

  long focusedMillis= pageState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
  focusedMillis= focusedMillis==0?System.currentTimeMillis():focusedMillis;
  pageState.addClientEntry("FOCUS_MILLIS",focusedMillis);

  PageSeed v = pageState.thisPage(request);
  v.command=Commands.SAVE;
  v.addClientEntry("FOCUS_MILLIS",focusedMillis);
  Form form= new Form(v);
  form.alertOnChange=true;

  form.start(pageContext);





 %><script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script>
<%
  ButtonLink adminLink = new ButtonLink(I18n.get("ADMINISTRATION_ROOT_MENU") + " /",pageState.pageFromRoot("administration/administrationIntro.jsp"));
%>
<%adminLink.toHtmlInTextOnlyModality(pageContext);%>
<h1><%=I18n.get("CONFIGURE_HOLIDAYS")%></h1>
<div class="container level_2"><%

  CompanyCalendar cc = new CompanyCalendar(new Date(focusedMillis));
  cc.set(CompanyCalendar.DAY_OF_MONTH,1);
  cc.set(CompanyCalendar.MONTH,CompanyCalendar.JANUARY);
  int oldMonth=-1;

// -------------------------------------------------------------------------------  START PREV AND NEXT -------------------------------------------------------------------
    ButtonSubmit prev = new ButtonSubmit(form);
    prev.variationsFromForm.addClientEntry("FOCUS_MILLIS", focusedMillis - CompanyCalendar.MILLIS_IN_YEAR);
    prev.label = "";
    prev.toolTip = I18n.get("PREVIOUS_YEAR");
    prev.iconChar = "{";

    ButtonSubmit next = new ButtonSubmit(form);
    next.variationsFromForm.addClientEntry("FOCUS_MILLIS", focusedMillis + CompanyCalendar.MILLIS_IN_YEAR);
    next.label = "";
    next.toolTip = I18n.get("NEXT_YEAR");
    next.iconChar = "}";

  %>
  <table class="table" cellpadding="0" cellspacing="0" border="0" style=" height:25px;">
    <tr>
      <td class="calHeader"><%prev.toHtmlInTextOnlyModality(pageContext);%></td>
      <td align="center" class="calHeader"><h2 style="margin: 0"><%=DateUtilities.dateToString(new Date(focusedMillis), "yyyy")%>
      </h2></td>
      <td align="right" class="calHeader"><%next.toHtmlInTextOnlyModality(pageContext);%></td>
    </tr>
  </table>
  <%// -------------------------------------------------------------------------  END PREV AND NEXT -------------------------------------------------------------------%>

<style type="text/css">
  .cell{
    cursor:pointer;
    font-size: 11px;
    width: auto;
  }

  .col0{
    background-color:#fff;
  }


  .weekend{
    background-color: #ffd5c0;
  }


  .col1{
    background-color:#ee8;
  }

  .col2{
    background-color: #ff6262;
  }



</style>


  <table class="table edged" id="calendar"><tr height="30"><th class="dayHeader"><%=I18n.get("MONTH")%></th><%
    for (int i=1 ; i<=31; i++){
      %><th width="3%" class="dayHeader"><%=i%></th><%
    }
  %></tr><%

  int year=cc.get(CompanyCalendar.YEAR);

  boolean swap=true;

  while (cc.get(CompanyCalendar.YEAR)== year){

    //test key break
    if (cc.get(CompanyCalendar.MONTH)!=oldMonth){
      oldMonth=cc.get(CompanyCalendar.MONTH);
      %><tr height="30"><td ><b><%=DateUtilities.dateToString(cc.getTime(),"MMMM")%></b></td><%
    }


    // 0= working day
    // 1= floating holyday
    // 2=fixed holiday
    int val=0;
    if (cc.isVariableHolyDay())
      val=1;
    else if (cc.isFixedHolyDay())
      val=2;

    boolean weekend= (cc.get(CompanyCalendar.DAY_OF_WEEK)==CompanyCalendar.FRIDAY && !cc.FRIDAY_IS_WORKING_DAY) ||
         (cc.get(CompanyCalendar.DAY_OF_WEEK)==CompanyCalendar.SATURDAY && !cc.SATURDAY_IS_WORKING_DAY) ||
        (cc.get(CompanyCalendar.DAY_OF_WEEK)==CompanyCalendar.SUNDAY && !cc.SUNDAY_IS_WORKING_DAY) ;

    String id="DAY_"+DateUtilities.dateToString(cc.getTime(), "yyyy_MM_dd");

    %><td class="cell <%=weekend?"weekend":""%> col<%=val%>" align="center" onclick="clickOnCell($(this))" title="<%=DateUtilities.dateToFullString(cc.getTime())%>">
      <%=DateUtilities.dateToString(cc.getTime(), "EEE")%>
      <input id="<%=id%>" name="<%=id%>"  type="hidden" value="<%=val%>" oldValue="1">
    </td><%
    cc.add(CompanyCalendar.DAY_OF_YEAR,1);
  }


  %></tr></table><%


  %><table class="legendaPlan" cellspacing="5"><tr height="25"><td valign="middle"><%=I18n.get("LEGENDA")%>:&nbsp;</td>
    <td width="30" class="col0" style="padding:3px;border:1px solid #aaa;">&nbsp;</td> <td><%=I18n.get("WORKING_DAY")%>&nbsp;&nbsp;&nbsp;</td>
    <td width="30" class="col1" style="padding:3px;border:1px solid #aaa;">&nbsp;</td> <td><%=I18n.get("HOLIDAY_VARIABLE")%>&nbsp;&nbsp;&nbsp;</td>
    <td width="30" class="col2" style="padding:3px;border:1px solid #aaa;">&nbsp;</td> <td><%=I18n.get("HOLIDAY_FIXED")%>&nbsp;&nbsp;&nbsp;</td>
    <td width="30" class="weekend" style="padding:3px;">&nbsp;</td> <td><%=I18n.get("WEEKEND")%></td>
    </tr></table><%


  ButtonBar bb=new ButtonBar();
  cc.setTimeInMillis(focusedMillis);
  ButtonJS save = new ButtonJS(I18n.get("SAVE"), "saveAll();");
  save.additionalCssClass="first";
  bb.addButton(save);
  bb.addSeparator(20);
  bb.addButton(new HtmlIncluder(I18n.get("EASTER_THIS_YEAR_IS")+": "+ DateUtilities.dateToFullString(cc.getEaster())));

    bb.toHtml(pageContext);
  %></div><%

  form.end(pageContext);


  %><script type="text/javascript">

  function clickOnCell(el){
    var input = el.find("input:first");
    var val=input.val();
    el.removeClass("col"+val);
    if (val==0)
      val=1;
    else if (val==1)
      val=2;
    else
      val=0;
    input.val(val);
    el.addClass("col"+val);
  }

  function saveAll(){
    showSavingMessage();

    var req= new Object();
    req["<%=Commands.COMMAND%>"]="SVHOL";
    $("#calendar .cell input").each(function(){
      var input=$(this);
      if (input.isValueChanged())
        req[input.prop("id")]=input.val();
    });


    $.getJSON('ajax/holidayController.jsp',req, function(response) {
      jsonResponseHandling(response);
      if (response.ok) {
        $("#calendar .cell input").updateOldValue();
        hideSavingMessage();
      }
    });

    //console.debug(req);


  }

  </script><%

}
%>