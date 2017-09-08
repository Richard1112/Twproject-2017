<%@ page import =" org.jblooming.page.Page,
                    org.jblooming.utilities.JSP,
                    org.jblooming.waf.html.core.JspIncluder,
                    org.jblooming.waf.html.core.JspIncluderSupport,
                    org.jblooming.waf.html.display.DataTable,
                    org.jblooming.waf.html.display.DataTable.TableHeaderElement, org.jblooming.waf.html.display.Paginator, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, java.util.List, org.jblooming.waf.constants.Fields, org.jblooming.tracer.Tracer"%><%

  PageState pageState = PageState.getCurrentPageState(request);
  DataTable dataTable = (DataTable) JspIncluderSupport.getCurrentInstance(request);
  Form f = dataTable.form;

  // -------------------------------  TABLE  ---------------------------------------------------------------------------------------------------------------------
if ("DRAW_TABLE".equals(request.getAttribute(Paginator.ACTION))) {

  //hidden field per il numero di pagina
  TextField tf = new TextField(Paginator.FLD_PAGE_NUMBER, "");
  tf.label="";
  tf.id=dataTable.id + "_PGtf";
  tf.type = "hidden";
  tf.preserveOldValue = false;
  tf.toHtml(pageContext);

%>
<table class="<%=dataTable.tableClass%> dataTable <%=dataTable.fixHead?"fixHead":""%> <%=dataTable.fixFoot?"fixFoot":""%>" id="<%=dataTable.id%>" formId="<%=f.id%>" <%=dataTable.bindReturnKeyOnForm?"bindReturnKey":""%> data-table="1" >
  <%dataTable.drawTableHeaders(pageContext);%>
  <%dataTable.drawTableRows(pageContext);%>
</table>
<%


  // -------------------------------  TABLE HEADERS ---------------------------------------------------------------------------------------------------------------------
} else  if ("DRAW_TBL_HEADERS".equals(request.getAttribute(Paginator.ACTION))) {
  Page dataPage = pageState.getPage();

%>
<%--%><thead class="dataTableHead" style="display:<%=dataPage==null || dataPage.getTotalNumberOfElements()<1?"none":"table-header-group"%>"><tr><%--%>
<thead class="dataTableHead" ><tr><%
  List headers = dataTable.headers;
  boolean first=true;
  for (Object o : headers) {
    if (o instanceof TableHeaderElement) {
      TableHeaderElement h= (TableHeaderElement) o;
      String style="";
      style+=JSP.ex(h.width)?"width:"+h.width+";":"";
      style+=JSP.ex(h.align)?"text-align:"+h.align+";":"";

      %><th class="tableHead" <%=JSP.ex(h.id)?"id=\""+h.id+"\"":""%> nowrap style="<%=style%>"><span class="tableHeadEl" <%if(JSP.ex(h.orderingHql)){%>orderState="<%=h.state%>" orderingHql="<%=h.orderingHql%>" onclick="dataTableChangeOrder($(this),'<%=dataTable.id%>')"<%}%>><%=h.label%></span></th><%
    } else if (o instanceof JspIncluder) {
      JspIncluder j= (JspIncluder) o;
      %><th class="tableHead" nowrap ><%j.toHtml(pageContext);%></th><%
    }
  }

  TextField hid = new TextField("hidden","",Form.FLD_FORM_ORDER_BY+ dataTable.id,"",0,false);
  //emulating form' generation of hiddenfields
  hid.preserveOldValue=false;
  hid.id = Form.FLD_FORM_ORDER_BY+ dataTable.id;

%>
</tr>
<tr style="display: none;"><td colspan="99"><%hid.toHtml(pageContext);%></td></tr>
</thead>

<%


  // -------------------------------  TABLE ROWS ---------------------------------------------------------------------------------------------------------------------
} else  if ("DRAW_TBL_ROWS".equals(request.getAttribute(Paginator.ACTION))) {

  //sul tbody sono riportati i dati di paginazione ed order by che arrivano dal controller.
  Page dataPage = pageState.getPage();
  if (dataPage==null || dataPage.getTotalNumberOfElements()<1){
%><tbody <%=dataPage==null?"noData":""%> class="dataTableBody" totalNumberOfElements="0" pageNumber="0" pageSize="0" ></tbody><%
} else {
%><tbody class="dataTableBody" totalNumberOfElements="<%=dataPage.getTotalNumberOfElements()%>" pageNumber="<%=dataPage.getPageNumber()%>" pageSize="<%=dataPage.getPageSize()%>" orderby="<%=JSP.w(pageState.getEntry(Form.FLD_FORM_ORDER_BY+dataTable.id).stringValueNullIfEmpty())%>"><%

  //Tracer.Profiler profiler = Tracer.getProfiler("DataTable_" + dataTable.id);
  //profiler.reset();
  List<Object> rows = dataPage.getThisPageElements();
  int i=0;
  for (Object obj : rows) {
    pageContext.getRequest().setAttribute(DataTable.ACTION, "DRAW_ROW");
    dataTable.rowDrawer.parameters.put("ROW_OBJ",obj);
    dataTable.rowDrawer.parameters.put("ROW_COUNT",i++);
    dataTable.rowDrawer.toHtml(pageContext);
  }
  //profiler.stop();
%></tbody><%

    if (dataTable.drawPageFooter) {
      pageContext.getRequest().setAttribute(DataTable.ACTION, "DRAW_PAGE_FOOTER");
      dataTable.rowDrawer.toHtml(pageContext);
    }

  }


  // -------------------------------  PAGINATOR ---------------------------------------------------------------------------------------------------------------------
} else  if ("DRAW_TBL_PAGINATOR".equals(request.getAttribute(Paginator.ACTION))) {
  Page dataPage = pageState.getPage();

%><div class="paginator unselectable" dataTableId="<%=dataTable.id%>">

  <%if (!"HIDEPAGES".equals(dataTable.parameters.get("COMMAND"))){%>
  <span class="paginatorPages"></span>
  <%}%>

  <div class="datatableInfo" >


  <%if (!"HIDETOOL".equals(dataTable.parameters.get("COMMAND"))){%>
    <div class="paginatorSearching"><%=I18n.get("SEARCHING_HELP")%></div>
    <div class="paginatorNotFound"><%=I18n.get("NO_RESULTS_HELP")%></div>
    <span class="paginatorFoundN"><span class="totalNumberOfElements"></span>&nbsp;<%=I18n.get("OBJECTS_FOUND")%></span>
    <span class="paginatorFound1">1 &nbsp;<%=I18n.get("OBJECT_FOUND")%>.</span>


    &nbsp;&nbsp;
  <div class="pagSize" >
    <span class="ruzzol teamworkIcon" style="cursor:pointer;" onclick="$(this).toggle();$(this).next().toggle().find(':input').focus();" title="<%=I18n.get("CHANGE_PAGE_SIZE")%>">g</span>
            <span class="pagSizeInp">
          <%
            if (dataPage!=null) {
              pageState.addClientEntry(Paginator.FLD_PAGE_SIZE, dataPage.getPageSize());
            } else {
              pageState.addClientEntry(Paginator.FLD_PAGE_SIZE, Paginator.getWantedPageSize(dataTable.id, pageState));
            }
            TextField psize = TextField.getIntegerInstance(Paginator.FLD_PAGE_SIZE);
            psize.label=I18n.get("OP_PAGE_SIZE");
            psize.separator= "&nbsp;";
            psize.fieldSize=3;
            psize.fieldClass = "formElements formElementsSmall";
            psize.preserveOldValue=  false;
            psize.autocomplete=false;
            psize.addKeyPressControl(13, "event.stopPropagation(); dataTableChangePageSize($('#"+dataTable.id+"'));", "onkeyup");
            psize.toHtml(pageContext);
          %></span>
  </div>
  <%} %>
  </div>

</div><%

  } else {

  }
%>
