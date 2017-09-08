<%@ page import="net.sf.json.JSONArray, net.sf.json.JSONObject, org.jblooming.operator.Operator, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.core.JST, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.input.LoadSaveFilter, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, java.util.Map, java.util.Set, java.util.TreeSet, org.jblooming.utilities.JSP" %><%
  PageState pageState = PageState.getCurrentPageState(request);
  Operator logged = pageState.getLoggedOperator();
  LoadSaveFilter lsfb = (LoadSaveFilter) JspIncluderSupport.getCurrentInstance(request);


  if (LoadSaveFilter.INITIALIZE.equals(request.getAttribute(LoadSaveFilter.ACTION))) {

} else  if ("DRAW_EDITOR".equals(request.getAttribute(LoadSaveFilter.ACTION))) {
%><span onclick="lsfOpenEditor($(this))" lsfId="<%=lsfb.id%>" title="<%=I18n.get("WANT_TO_SAVE_FILTER")%>" class="button textual"><%=JSP.w(lsfb.label)%></span><%

} else if ("DRAW_BUTTONS".equals(request.getAttribute(LoadSaveFilter.ACTION))){

  Map<String,String> filterMap = logged.getFilters();

  if (logged != null ) {
    JSONArray ja = new JSONArray();
    Set<String> flts = new TreeSet();
    flts.addAll(filterMap.keySet());
    String selFilter = pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();
    for (String key : flts) {
      if (key.startsWith(lsfb.category)) {
        String filterName = key.substring(lsfb.category.length());
        JSONObject jo = new JSONObject();
        if (filterName.equals(selFilter)) {
          jo.element("selected", true);
        }
        jo.element("name", filterName);
        ja.add(jo);
      }
    }

%><div class="customSavedFilters" formId="<%=lsfb.form.id%>" category="<%=lsfb.category%>" id="<%=lsfb.id%>">
  <input type="hidden" name="FLNMSEL" id="FLNMSEL">
  <input type="hidden" name="<%=Fields.FLD_FILTER_CATEGORY%>" id="<%=Fields.FLD_FILTER_CATEGORY%>" value="<%=lsfb.category%>">
  <input type="hidden" name="<%=Fields.FLD_FILTER_NAME%>" id="<%=Fields.FLD_FILTER_NAME%>" value="">
</div>
<script type="text/javascript">
  $(function(){
    $("#<%=lsfb.form.id%>").append($("#<%=lsfb.id%> input")); //sposta gli input dentro il form
    var bts=<%=ja.toString()%>;
    var div=$("#<%=lsfb.id%>");
    for (var i=0;i<bts.length;i++){
      div.append(lsfCreateButton(bts[i]));
    }
  });
</script>


<%

    }
  }
%>
