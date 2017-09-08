<%@ page import="org.jblooming.utilities.JSP,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.button.ButtonSupport,
                 org.jblooming.waf.html.container.Tab,
                 org.jblooming.waf.html.container.TabSet,
                 org.jblooming.waf.html.core.JspIncluder,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.input.TextField, org.jblooming.waf.view.PageState" %>
<%

 if (TabSet.BAR.equals(request.getAttribute(Commands.COMMAND))) {

  PageState pageState= PageState.getCurrentPageState(request);
  TabSet tabSet = (TabSet) JspIncluderSupport.getCurrentInstance(request);

  String focusedId=pageState.getEntry(tabSet.id).stringValueNullIfEmpty();
%>
<div class="tabSetBox" style="<%=tabSet.style%>">
  <%
    TextField tf = new TextField("hidden", tabSet.id, "", 20);
    tf.preserveOldValue = false;
    tf.label = "";
    tf.toHtml(pageContext);

  %>
  <div class="tabSetHeader noprint">
    <div class="tabSetPre"><% for (JspIncluder jspi : tabSet.pre) {
      %><div class="tabPre"><%
      jspi.toHtml(pageContext);
        %></div><%
    } %></div>
    <div class="tabSetTabs" id="TABSETTABS_<%=tabSet.id%>"><%

      for (Tab tab : tabSet.tabs) {
        String clickTab = ";clickTab('" + tab.id + "');";
        if (tab.id.equals(focusedId))
          tab.focused=true;

        // if no button specified ->  create a js ones
        if (tab.button == null) {
          ButtonJS bjs = new ButtonJS(clickTab);
          if (JSP.ex(tab.additionalScript))
            bjs.onClickScript += tab.additionalScript;
          bjs.label = tab.caption;
          bjs.enabled = tab.enabled;
          bjs.additionalCssClass=tab.additionalCssClass;
          tab.button = bjs;

        } else {
          // does not append clickTab, usually when there is a button it is a SubmitButton. But is this true always? We will see sooner hahahahahahahahahah!
          // this prevents swap before to re-load page
          //tab.button.additionalOnClickScript = JSP.w(tab.button.additionalOnClickScript)+clickTab;
          tab.enabled = tab.button.enabled;
        }

        tab.button.outputModality = ButtonSupport.TEXT_ONLY;

        String buttonClass = "";
        if (tab.focused)
          buttonClass = "tabSelected";
        else if (!tab.enabled)
          buttonClass = "tabDisabled";

        buttonClass=buttonClass+ (JSP.ex(tab.additionalCssClass)?" "+tab.additionalCssClass:"");

    %>
      <div id="<%=tab.id%>" tabset="<%=tabSet.id%>" class="<%=buttonClass%> tab" style="<%=tab.enabled ? "cursor:pointer;" : "cursor:default"%>"  <%=tab.generateToolTip()%>>
        <%tab.button.toHtml(pageContext);%>
      </div>
      <%}%>
    </div>
    <div class="tabSetPost"><% for (JspIncluder jspi : tabSet.post) {
    %><div class="tabPost"><%
      jspi.toHtml(pageContext);
    %></div><%
    } %></div>
  </div>
  <div class="tabContainer clearfix"  id="TABSETPART_<%=tabSet.id%>" >
    <%

    } else if (TabSet.END.equals(request.getAttribute(Commands.COMMAND))) {

    %>
  </div>
</div>
<%

} else if (Tab.START.equals(request.getAttribute(Commands.COMMAND))) {
  Tab tab = (Tab) JspIncluderSupport.getCurrentInstance(request);

%><!-- TAB <%=tab.caption%> START -->
<div class="tabBox" tabSetId="<%=tab.tabSet.id%>" tabId="<%=tab.id%>" style="<%= tab.focused ? "display:block;" : "display:none;"%>" ><%

} else if (Tab.END.equals(request.getAttribute(Commands.COMMAND))) {
%></div>
<%

  }
%>
