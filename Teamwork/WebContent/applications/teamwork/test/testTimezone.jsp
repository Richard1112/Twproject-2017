<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.waf.TeamworkHBFScreen, org.jblooming.waf.ScreenArea, org.jblooming.waf.view.PageState, java.util.*" %>

<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {


List<TimeZone> tzs = new ArrayList<TimeZone>();
for (String tzid : TimeZone.getAvailableIDs())
  tzs.add(TimeZone.getTimeZone(tzid));

Collections.sort(tzs, new Comparator<TimeZone>() {
  public int compare(TimeZone o1, TimeZone o2) {
  return new Integer(o1.getRawOffset()).compareTo(o2.getRawOffset());
  }
});


%>
<style type="text/css">
  .tz_selector{
    width: 200px;
    height: 300px;
    overflow: hidden;

    border:1px solid #e0e0e0;
  }

  .tz_options{
    width: 100%;
    height: 290px;
    overflow-y: auto;
    overflow-x: hidden;
  }

  .tz_search{
    width: 100%;
    height: 20px;
    padding: 2px;
  }

  .tz_option{
    overflow: hidden;
    width: 1000px;
  }
  .tz_option small{
    font-size:8px;
    width: 35px;
    display: inline-block;
  }

</style>
<script type="text/javascript">
  function tz_search(el){

    el.stopTime("tz_refresh");
    el.oneTime(200,"tz_refresh",function(){
      var sstr= $(this).val().trim().toUpperCase();
      if (sstr!=""){
        var found=0;
        $(".tz_option").each(function(){
          var opt=$(this);
          if (opt.text().toUpperCase().indexOf(sstr)>=0 ){
            opt.fadeIn();
            found++;
          } else {
            opt.fadeOut();
          }
        });

        if (found==1){
          $("#timeZone").val($(".tz_option:visible").attr("tzId"));
        } else {
          $("#timeZone").val("");
        }

      } else {
        $(".tz_option").fadeIn();
      }

    })

  }
</script>
<div class="tz_selector">
  <input type="text" class="tz_search" onkeyup="tz_search($(this))">
  <input type="hidden" name="TIMEZONE" id="TIMEZONE">
  <div class="tz_options">

    <%
      for (TimeZone tz:tzs){
        int offset = (int) (tz.getRawOffset() / 3600000);
        String offS = offset > 0 ? "+" + offset : (offset < 0 ? "" + offset : "");
        %><div class="tz_option" tzId="<%=tz.getID()%>"><small >(GMT<%=offS%>)</small> <%=tz.getID()%></div><%
      }


    %>


  </div>
</div>
<%
  }
%>