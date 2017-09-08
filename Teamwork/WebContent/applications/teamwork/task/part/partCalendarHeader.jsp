<%@ page import="org.jblooming.agenda.CompanyCalendar, org.jblooming.agenda.Scale, org.jblooming.utilities.DateUtilities, org.jblooming.waf.SessionState, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  Form form = pageState.getForm();

  // get the focused millis
  long focusedMillis= pageState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
  focusedMillis= focusedMillis==0?System.currentTimeMillis():focusedMillis;
  pageState.addClientEntry("FOCUS_MILLIS",focusedMillis);

  Scale.ScaleType scaleType = Scale.ScaleType.SCALE_1WEEK;
  Scale scale = Scale.getScaleAndSynch(scaleType, focusedMillis, true, SessionState.getLocale());


  final long minMillisInBar = scale.startPointTime;

%>
<table width="100%" cellspacing="0" cellpadding="0" border="0">
  <tr>
    <td class="calHeader left">
        <span class="button textual today noprint" onclick="goToMillis(new Date().getTime())"><%=I18n.get("TODAY")%></span>
        <div style="float:right"><span class="button textual noprint icon" onclick="goToMillis(millis - 3600000 * 24 * 7)"><span class="teamworkIcon" style="font-size:18px">{</span></span>
        <span class="button textual noprint icon" onclick="goToMillis(millis + 3600000 * 24 * 7)"><span class="teamworkIcon" style="font-size:18px">}</span></span>
        </div>
    </td>
    <td class="calHeader">
      <h2 style="margin:0">
      <div style="position:relative;height:30px;">
        <div id="topHeaderCentral"><%
          CompanyCalendar cc = new CompanyCalendar();
          cc.setTimeInMillis(minMillisInBar);
        %><%=DateUtilities.dateToString(cc.getTime(), "dd MMMM")%>&nbsp;-&nbsp;<%
          cc.add(CompanyCalendar.WEEK_OF_YEAR, 1);
          cc.add(CompanyCalendar.MILLISECOND, -1);
        %><%=DateUtilities.dateToString(cc.getTime(), "dd MMMM yyyy")%><sup>(<%=DateUtilities.dateToString(cc.getTime(), "ww")%>)</sup>
        </div>
        <div class="headerCalendarOpener" title="<%=I18n.get("DATEFIELDCALENDAR")%>" id="openCal" onclick="$(this).dateField({inputField:$('#dummy'),callback:function(date){goToMillis(date.getTime());}}); "><input type="hidden" id="dummy"><span class="teamworkIcon">m</span></div>
      </div></h2>
    </td>
  </tr>
</table>
<script type="text/javascript">

  var millis =<%=focusedMillis%>;

</script>
