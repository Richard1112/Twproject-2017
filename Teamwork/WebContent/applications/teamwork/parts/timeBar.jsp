<%@ page import="net.sf.json.JSONArray, org.jblooming.agenda.Period, org.jblooming.agenda.Scale,
org.jblooming.utilities.JSP, org.jblooming.waf.SessionState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, java.text.ParseException,
 java.util.List" %><%

  PageState pageState = PageState.getCurrentPageState(request);

  long focusedMillis= pageState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
  focusedMillis= focusedMillis==0?System.currentTimeMillis():focusedMillis;
  pageState.addClientEntry("FOCUS_MILLIS",focusedMillis);

  // get the best fitting scale and increment of ticks
  Scale scale = Scale.getScaleAndSynch(Scale.ScaleType.SCALE_2YEAR, focusedMillis, false, SessionState.getLocale());

  //start & end of displayed week
  Period period = (Period) pageState.attributes.get("FOCUSED_PERIOD");


  // HIGHLIGHTED PERIODS
  List<Period> hlpps = (List<Period>) pageState.attributes.get("HIGHLIGHT_PERIODS");
  JSONArray periods=new JSONArray();
  if (JSP.ex(hlpps)){
    for (Period hl:hlpps){
      periods.add(hl.jsonify());
    }
  }

%>
<div class="bottomBar" id="moveBar" onclick="moveBarClick($(this),event);"></div>
<script type="application/javascript">

  $(function() {
    var moveBar = $("#moveBar");
    moveBar.empty();

    var startMillis = <%=scale.startPointTime%>;
    var endMillis = <%=scale.endPointTime%>;

    var barFolio = new Folio(moveBar);
    barFolio.width = endMillis - startMillis;
    barFolio.height = 50;
    barFolio.left = startMillis;
    barFolio.inPercent=true;

    var d = new Date(startMillis);
    while (d.getTime() < endMillis) {
      var headLbl = $("<span>").html(d.format("MM yyyy")).addClass("moveBarEl");
      barFolio.addElement(headLbl, 0, d.getTime(), 30 * 24 * 3600000, 50);
      d.setMonth(d.getMonth() + 1);
    }

    var periods=<%=periods%>;
    for (var i=0;i<periods.length;i++) {
      var highlight = $("<span>").css({"background-color":"#BCFF3A",opacity:.6});
      barFolio.addElement(highlight, 35, periods[i].startMillis, periods[i].endMillis-periods[i].startMillis, 15);
    }

    //start & end of displayed week
    var period = $("<span>").addClass("moveBarHL").prop("title","<%=I18n.get("DISPLAYED_PERIOD")%>");
    barFolio.addElement(period, 25, <%=period.getValidityStartTime()%>, <%=period.getValidityEndTime()-period.getValidityStartTime()%>, 25);

    //today
    var today = $("<span>").prop("title", "<%=I18n.get("TODAY")%>").addClass("moveBarToday");
    barFolio.addElement(today, 0, new Date().getTime(), barFolio.getPixelWidth() * 2, 50);

    barFolio.redraw();
    return barFolio;
  });




  function moveBarClick(el, event) {
    var pos = event.clientX - el.offset().left;
    var folio = el.data("folio");
    var millis = parseInt(folio.getVirtualLeft(pos));
    goToMillis(millis);
  }


</script>
