<%@ page import="org.jblooming.agenda.Scale, org.jblooming.utilities.DateUtilities, org.jblooming.waf.SessionState, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jblooming.waf.html.button.ButtonSubmit, com.sun.imageio.plugins.common.I18N" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);
  Form form = pageState.getForm();
  Scale.ScaleType scaleType = Scale.ScaleType.valueOf(pageState.getEntryOrDefault("SCALE_TYPE").stringValueNullIfEmpty());
  // get the focused millis
  long focusedMillis= pageState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
  focusedMillis= focusedMillis==0?System.currentTimeMillis():focusedMillis;
  pageState.addClientEntry("FOCUS_MILLIS",focusedMillis);

  Scale scale = Scale.getScaleAndSynch(scaleType, focusedMillis, true, SessionState.getLocale());

// -------------------------------------------------------------------------------  START PREV AND NEXT -------------------------------------------------------------------

%>


<table cellspacing="0" cellpadding="0" style="width: 100%">
  <tr>
    <td class="calHeader left">
        <span class="button textual today noprint" onclick="goToMillis(new Date().getTime());"><%=I18n.get("TODAY")%></span>
        <div style="float:right">
        <span class="button textual icon noprint" onclick="goToMillis(<%=scale.startPointTime-(scale.endPointTime-scale.startPointTime)/2 %>);"><span class="teamworkIcon" style="font-size:18px">{</span></span>
        <span class="button textual icon noprint" onclick="goToMillis(<%=scale.endPointTime+3600000*3%>);"><span class="teamworkIcon" style="font-size:18px">}</span></span>
         </div>
    </td>

    <td class="calHeader">
      <h2 style="margin:0">
    <div style="position:relative;">
      <div class="unselectable" unselectable="true" id="topHeaderCentral"><%
        String sm = DateUtilities.dateToString(scale.startPointDate, "MMMM yyyy")+ "<sup>("+DateUtilities.dateToString(scale.startPointDate, "ww") +")</sup>";
        String em = DateUtilities.dateToString(scale.endPointDate, "MMMM yyyy")+ "<sup>("+DateUtilities.dateToString(scale.endPointDate, "ww") +")</sup>";

        if (!sm.equals(em)){
          sm=sm+ " / "+em;
        }
          %><%=sm%>
      </div>
      <div class="headerCalendarOpener" title="<%=I18n.get("DATEFIELDCALENDAR")%>" id="openCal" onclick="$(this).dateField({inputField:$('#dummy'),callback:function(date){goToMillis(date.getTime());}}); "><input type="hidden" id="dummy"><span class="teamworkIcon">m</span></div>
    </div></h2>
  </td>
  <td class="calHeader right" style="width:300px">
    <%

      ButtonSubmit one=new ButtonSubmit("1 "+ I18n.get("WEEK"),"",pageState.getForm());
      one.variationsFromForm.addClientEntry("SCALE_TYPE",Scale.ScaleType.SCALE_1WEEK+"");
      one.additionalCssClass=scaleType.equals(Scale.ScaleType.SCALE_1WEEK)?"focused":"";
      one.toHtmlInTextOnlyModality(pageContext);

    %>&nbsp;&nbsp;&nbsp;<%

    ButtonSubmit two=new ButtonSubmit("2 "+ I18n.get("WEEKS"),"",pageState.getForm());
    two.variationsFromForm.addClientEntry("SCALE_TYPE",Scale.ScaleType.SCALE_2WEEK+"");
    two.additionalCssClass=scaleType.equals(Scale.ScaleType.SCALE_2WEEK)?"focused":"";
    two.toHtmlInTextOnlyModality(pageContext);

  %>&nbsp;&nbsp;&nbsp;<%

    ButtonSubmit three=new ButtonSubmit("3 "+ I18n.get("WEEKS"),"",pageState.getForm());
    three.variationsFromForm.addClientEntry("SCALE_TYPE",Scale.ScaleType.SCALE_3WEEK+"");
    three.additionalCssClass=scaleType.equals(Scale.ScaleType.SCALE_3WEEK)?"focused":"";
    three.toHtmlInTextOnlyModality(pageContext);

  %>&nbsp;&nbsp;&nbsp;<%

    ButtonSubmit four=new ButtonSubmit("1 "+I18n.get("MONTH"),"",pageState.getForm());
    four.variationsFromForm.addClientEntry("SCALE_TYPE", Scale.ScaleType.SCALE_1MONTH+"");
    four.additionalCssClass=scaleType.equals(Scale.ScaleType.SCALE_1MONTH)?"focused":"";
    four.toHtmlInTextOnlyModality(pageContext);


    %>

  </td>
</tr>
</table>

  <script type="text/javascript">
    function goToMillis(newMillis) {
      $("[name=FOCUS_MILLIS]").val(newMillis);
      $("#<%=form.id%>").submit();
    }
  </script>

<%// -------------------------------------------------------------------------  END PREV AND NEXT -------------------------------------------------------------------%>
