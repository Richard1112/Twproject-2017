<%@ page import="org.jblooming.agenda.CompanyCalendar, org.jblooming.utilities.CodeValueList, org.jblooming.utilities.DateUtilities, org.jblooming.waf.SessionState, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.input.*, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, java.util.Calendar, java.util.Locale" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  ScheduleComposer composer = (ScheduleComposer) JspIncluderSupport.getCurrentInstance(request);


  if (ScheduleComposer.DRAW_INPUT.equals(request.getAttribute(ScheduleComposer.ACTION))) { //  DRAW COMPONENT
%>
<div class="schedComposer">
<%
  TextField tf = new TextField("hidden",composer.id,"",1);
  tf.label = "";
  tf.fieldSize=150;
  tf.fieldClass="scMainField";
  tf.toHtml(pageContext);


  CodeValueList daysOfWeek = ScheduleComposer.getDaysOfWeek(pageState);
  CodeValueList weeksOfMonth = ScheduleComposer.getWeeksOfMonth(pageState);
  CodeValueList days = new CodeValueList();
  for (int i = 1; i < 32; i++) {
    days.add(String.valueOf(i),String.valueOf(i));
  }



  ComboBox timeBox = ComboBox.getTimeInstance("", "", "TIMECMB", 0, pageState);
  timeBox.id="";
  timeBox.onSelectScript = "jsonifySchedule($(this));";
  timeBox.script="onChange='jsonifySchedule($(this));'";
  timeBox.separator = "<br>";
  timeBox.readOnly=composer.readOnly;

%>

  <%-- -------------------------------------------------- PERIOD --------------------------------------------------------------%>
<div class="schedFields">
  <table class="sc_period" border="0"> <%


    %><tr><td nowrap valign="bottom"><%
    DateField startField = new DateField("startDate");
   /* startField.labelstr=I18n.get("START_DATE");*/
    startField.labelstr=I18n.get("START");
    startField.id="";
    startField.onblurOnDateValid="jsonifySchedule($(this));";
    startField.size=8;
    startField.separator="<br>";
    startField.preserveOldValue=false;
    startField.readOnly=composer.readOnly;
    startField.toHtml(pageContext);

    %>&nbsp;&nbsp;</td><td nowrap valign="bottom"><%

    timeBox.fieldName="startHour";
    timeBox.label=""/*I18n.get("START_HOUR")*/;
    timeBox.toolTip=I18n.get("START_HOUR");
    timeBox.preserveOldValue=false;
    timeBox.toHtml(pageContext);

  %></td><td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>
    <td nowrap valign="bottom"><div id="sc_endDate"><%
      DateField endField = new DateField("endDate");
      /*endField.labelstr=I18n.get("END_DATE");*/
      endField.labelstr=I18n.get("END");
      endField.id="";
      endField.size=8;
      endField.onblurOnDateValid="jsonifySchedule($(this));";
      endField.separator="<br>";
      endField.preserveOldValue=false;
      endField.readOnly=composer.readOnly;
      endField.toHtmlI18n(pageContext);


    %>&nbsp;&nbsp;</div></td><td nowrap valign="bottom"><div id="sc_endHour"><%

      timeBox.fieldName="endHour";
      timeBox.label=""/*I18n.get("END_HOUR")*/;
      timeBox.toolTip=I18n.get("END_HOUR");
      timeBox.preserveOldValue=false;
      timeBox.toHtml(pageContext);


    %>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</div></td>
    <td valign="bottom" nowrap ><%

    ComboBox timeBoxD = ComboBox.getTimeInstance("", "", "TIMECMBD", 900000, pageState);
    timeBoxD.id="";
    timeBoxD.onSelectScript = "jsonifySchedule($(this));";
    timeBoxD.script="onChange='jsonifySchedule($(this));'";
    timeBoxD.separator = "<br>";
    timeBoxD.fieldName="duration";
    timeBoxD.label=I18n.get("DURATION");
    timeBoxD.preserveOldValue=false;
    timeBoxD.readOnly=composer.readOnly;
    timeBoxD.toHtml(pageContext);


  %></td><td valign="bottom" style="padding-bottom: 5px">
      <span id="sc_fullDay"><input type="checkbox" onclick="setFullDay($(this));" <%=composer.readOnly?"disabled":""%>> <%=I18n.get("ALL_DAY")%></span>
  </td></tr></table>

    <%-- --------------------------------------------------------------- MINUTE -----------------------------------------------------------------------------%>
  <div id="sc_recurMinute"  recurr="minute" class="sc_recurr" style="display:none;">
    <div class="separator"></div>
    <input type="checkbox" name="onlyWorkingDays" onclick="jsonifySchedule($(this));" <%=composer.readOnly?"readonly":""%>><%=I18n.get("AGE_WORKING_DAYS_ONLY")%> &nbsp;&nbsp;
  <%
    tf = new TextField(I18n.get("AGE_RECURRENT_MINUTE_EVERY"),"freq", "&nbsp;", 2, false);
    tf.script="onblur=\"jsonifySchedule($(this));\"";
    tf.preserveOldValue=false;
    tf.readOnly=composer.readOnly;
    tf.toHtml(pageContext);
  %>&nbsp;&nbsp;<%
    tf = new TextField(I18n.get("AGE_RECURRENT_HOWMANYTIMES"), "repeat", "&nbsp;", 2, false);
    tf.script="onblur=\"jsonifySchedule($(this));\"";
    tf.preserveOldValue=false;
    tf.readOnly=composer.readOnly;
    tf.toHtml(pageContext);

  %><%=I18n.get("AGENDA_TIMES")%></div>



  <div class="scheduleTypes" style="padding: 10px 0 10px 0;font-size:12px; ">
    <%
      if (!composer.readOnly){
                
        if (composer.isSingle) {
      %><span class="button textual" schedType="period" onclick="changeSchedType($(this));"><%=I18n.get("SCHEDULE_NO_REPEAT")%></span><%
      }
    %>&nbsp;&nbsp;<span style="color:#666"><%=I18n.get("SCHEDULE_REPEAT")%>:</span>&nbsp;&nbsp;<%
      if (composer.isMinute) {
    %><span class="button textual" schedType="minute" onclick="changeSchedType($(this));"><%=I18n.get("SCHED_MINUTE")%></span>&nbsp;&nbsp;<%
      }

      if (composer.isDaily) {
    %><span class="button textual" schedType="daily" onclick="changeSchedType($(this));"><%=I18n.get("SCHED_DAILY")%></span>&nbsp;&nbsp;<%
      }

      if (composer.isWeekly) {
    %><span class="button textual" schedType="weekly" onclick="changeSchedType($(this));"><%=I18n.get("SCHED_WEEKLY")%></span>&nbsp;&nbsp;<%
      }

      if (composer.isMonthly) {
    %><span class="button textual" schedType="monthly" onclick="changeSchedType($(this));"><%=I18n.get("SCHED_MONTHLY")%></span>&nbsp;&nbsp;<%
      }

      if (composer.isYearly) {
    %><span class="button textual" schedType="yearly" onclick="changeSchedType($(this));"><%=I18n.get("SCHED_YEARLY")%></span>&nbsp;&nbsp;<%
      }
    }
  %></div>




    <%-- --------------------------------------------------------------- DAILY -----------------------------------------------------------------------------%>
    <div id="sc_recurDaily"  recurr="daily" class="sc_recurr" style="display:none;">
      <div class="separator"></div>
      <input type="checkbox" name="onlyWorkingDays" onclick="jsonifySchedule($(this));" <%=composer.readOnly?"readonly":""%>><%=I18n.get("AGE_WORKING_DAYS_ONLY")%> &nbsp;&nbsp;
      <%
      tf = new TextField(I18n.get("AGE_RECURRENT_DAILY_EVERY"),"freq", "&nbsp;", 2, false);
      tf.script="onblur=\"jsonifySchedule($(this));\"";
      tf.preserveOldValue=false;
      tf.fieldClass="formElements time";
      tf.readOnly=composer.readOnly;
      tf.toHtml(pageContext);
    %>&nbsp;<%=I18n.get("DAYS")%>&nbsp;&nbsp;&nbsp;&nbsp;<%
      tf = new TextField(I18n.get("AGE_RECURRENT_HOWMANYTIMES"), "repeat", "&nbsp;", 2, false);
      tf.script="onblur=\"jsonifySchedule($(this));\"";
      tf.preserveOldValue=false;
      tf.fieldClass="formElements time";
      tf.readOnly=composer.readOnly;
      tf.toHtml(pageContext);
    %>&nbsp;<%=I18n.get("AGENDA_TIMES")%>
    </div>

    <%-- --------------------------------------------------------------- WEEKLY -----------------------------------------------------------------------------%>
    <div id="sc_recurWeekly"  recurr="weekly" class="sc_recurr" style="display:none;">
      <div class="separator"></div><%
          tf = new TextField(I18n.get("AGE_RECURRENT_WEEKLY_EVERY"), "freq", "&nbsp;", 2, false);
          tf.script="onblur=\"jsonifySchedule($(this));\"";
          tf.preserveOldValue=false;
          tf.fieldClass="formElements time";
          tf.readOnly=composer.readOnly;
          tf.toHtml(pageContext);

        %>&nbsp;<%=I18n.get("WEEKS")%>&nbsp;&nbsp;&nbsp;&nbsp;<%

          tf = new TextField(I18n.get("AGE_RECURRENT_HOWMANYTIMES"), "repeat", "&nbsp;", 2, false);
          tf.script="onblur=\"jsonifySchedule($(this));\"";
          tf.preserveOldValue=false;
          tf.fieldClass="formElements time";
          tf.readOnly=composer.readOnly;
          tf.toHtml(pageContext);

        %>&nbsp;<%=I18n.get("IN_THE_WEEK_DAYS")%>:&nbsp;&nbsp;<%
      
          Locale locale = SessionState.getLocale();
          CompanyCalendar cal = new CompanyCalendar(locale);
          cal.set(Calendar.DAY_OF_WEEK, cal.getFirstDayOfWeek());

          for (int i = 0; i < 7; i++) {
            %><input type="checkbox" onclick="jsonifySchedule($(this));" name="day<%=cal.get(CompanyCalendar.DAY_OF_WEEK)%>" day="<%=cal.get(CompanyCalendar.DAY_OF_WEEK)%>" <%=composer.readOnly?"readonly":""%>> <%=DateUtilities.dateToString(cal.getTime(), "EE")%> &nbsp;&nbsp;<%
            cal.add(CompanyCalendar.DAY_OF_WEEK, 1);
          }
        %>
      </div>

    <%-- --------------------------------------------------------------- MONTHLY -----------------------------------------------------------------------------%>
    <div id="sc_recurMonthly"  recurr="monthly" class="sc_recurr" style="display:none;">
      <div class="separator"></div>
      <table border="0"><tr><td>
        <%
          RadioButton rb = new RadioButton(I18n.get("AGE_RECURRENT_MONTHLY"), "recurType", "1", "", null, false, "");
          rb.id="";
          rb.script="jsonifySchedule($(this));";
          rb.preserveOldValue=false;
          rb.readOnly=composer.readOnly;
          rb.toHtml(pageContext);

        %>&nbsp;<span class="monthlyDate"></span>&nbsp;<%=I18n.get("AGE_RECURRENT_MONTHLY_MONTHSNUMBER")%>&nbsp;&nbsp;&nbsp;&nbsp;
        </td>
        <td><%
          rb = new RadioButton(I18n.get("AGE_RECURRENT_MONTHLY"), "recurType", "2", "", null, false, "");
          rb.id="";
          rb.script="jsonifySchedule($(this));";
          rb.preserveOldValue=false;
          rb.readOnly=composer.readOnly;
          rb.toHtml(pageContext);
        %>&nbsp;<%
          Combo cbb = new Combo("weekOfMonth","&nbsp;", "formElements", 10, "", weeksOfMonth, "");
          cbb.label = "";
          cbb.onBlurScript="jsonifySchedule($(this));";
          cbb.preserveOldValue=false;
          cbb.readOnly=composer.readOnly;
          cbb.toHtml(pageContext);
        %>&nbsp;<%
          cbb = new Combo("dayOfWeek", "&nbsp;", "formElements", 10, "", daysOfWeek, "");
          cbb.label = "";
          cbb.onBlurScript="jsonifySchedule($(this));";
          cbb.preserveOldValue=false;
          cbb.readOnly=composer.readOnly;
          cbb.toHtml(pageContext);
        %>&nbsp;&nbsp;&nbsp;&nbsp;
        </td>
          <td colspan="2" valign="middle"><%
              tf = new TextField(I18n.get("AGE_RECURRENT_MONTHLY"), "freq", "&nbsp;", 2, false);
              tf.script="onblur=\"jsonifySchedule($(this));\"";
              tf.preserveOldValue=false;
              tf.fieldClass="formElements time";
              tf.readOnly=composer.readOnly;
              tf.toHtml(pageContext);
          %>&nbsp;<%=I18n.get("MONTHS")%>&nbsp;&nbsp;&nbsp;&nbsp;<%
              tf = new TextField(I18n.get("AGE_RECURRENT_HOWMANYTIMES"), "repeat", "&nbsp;", 2, false);
              tf.script="onblur=\"jsonifySchedule($(this));\"";
              tf.preserveOldValue=false;
              tf.fieldClass="formElements time";
              tf.readOnly=composer.readOnly;
              tf.toHtml(pageContext);
          %>&nbsp;<%=I18n.get("AGENDA_TIMES")%>
          </td>
        </tr>
        </table>
    </div>


    <%-- --------------------------------------------------------------- YEARLY -----------------------------------------------------------------------------%>
    <div id="sc_recurYearly" recurr="yearly" class="sc_recurr"  style="display:none;">
      <div class="separator"></div>
      <table border="0"><tr>
          <td>
        <%
          rb = new RadioButton(I18n.get("AGE_RECURRENT_YEARLY"),"recurType", "1", "", null, false, "");
          rb.id="";
          rb.script="jsonifySchedule($(this));";
          rb.preserveOldValue=false;
          rb.readOnly=composer.readOnly;
          rb.toHtml(pageContext);

        %>&nbsp;<span class="yearlyDate1"></span>&nbsp;&nbsp;&nbsp;&nbsp;
        </td>
        <td><%
          rb = new RadioButton(I18n.get("AGE_RECURRENT_YEARLY"), "recurType", "2", "", null, false, "");
          rb.id="";
          rb.script="jsonifySchedule($(this));";
          rb.preserveOldValue=false;
          rb.readOnly=composer.readOnly;
          rb.toHtml(pageContext);

        %>&nbsp;<%

          cbb = new Combo("weekOfMonth", "&nbsp;", "formElements", 10, "", weeksOfMonth, "");
          cbb.label = "";
          cbb.onBlurScript="jsonifySchedule($(this));";
          cbb.preserveOldValue=false;
          cbb.readOnly=composer.readOnly;
          cbb.toHtml(pageContext);

        %>&nbsp;&nbsp;<%

          cbb = new Combo("dayOfWeek", "&nbsp;", "formElements", 10, "", daysOfWeek, "");
          cbb.label = "";
          cbb.onBlurScript="jsonifySchedule($(this));";
          cbb.preserveOldValue=false;
          cbb.readOnly=composer.readOnly;
          cbb.toHtml(pageContext);

        %>&nbsp;<%=I18n.get("OF")%>&nbsp;<span class="yearlyDate2"></span>&nbsp;&nbsp;&nbsp;&nbsp;

        </td>
          <td valign="middle">

              <input type="hidden" value="1" name="freq">

              <%

                  tf = new TextField(I18n.get("AGE_RECURRENT_HOWMANYTIMES"), "repeat",  "&nbsp;", 2, false);
                  tf.script="onblur=\"jsonifySchedule($(this));\"";
                  tf.preserveOldValue=false;
                  tf.fieldClass="formElements time";
                  tf.readOnly=composer.readOnly;
                  tf.toHtml(pageContext);

              %>&nbsp;<%=I18n.get("AGENDA_TIMES")%>
          </td>
        </tr>

          </table>
    </div>
</div>


</div>
<%
}
%>
