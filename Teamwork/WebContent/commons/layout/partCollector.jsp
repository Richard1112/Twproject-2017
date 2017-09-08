<%@ page import="org.jblooming.waf.SessionState,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.input.CheckBox,
                 org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.input.Collector,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.view.ClientEntries,
                 org.jblooming.ontology.PlatformComparators.ClientEntryComparator,
                 org.jblooming.waf.view.PageState,
                 java.util.ArrayList,
                 java.util.Collections, java.util.Iterator, java.util.List" %><%

  PageState pageState = PageState.getCurrentPageState(request);

  Collector collector = (Collector) JspIncluderSupport.getCurrentInstance(request);
  final String id = collector.id;
  Form form = collector.form;
  ClientEntries ces = pageState.getClientEntries();

  int totColumns = 2;
  totColumns = totColumns + (collector.checkBoxes == null ? 0 : collector.checkBoxes.size());

/*
________________________________________________________________________________________________________________________________________________________________________


sub parts

________________________________________________________________________________________________________________________________________________________________________

*/
  final boolean drawCand = Collector.DRAW_CANDIDATES.equals(request.getAttribute(Collector.ACTION));
  final boolean drawChs = Collector.DRAW_CHOSEN.equals(request.getAttribute(Collector.ACTION));

  if (drawCand || drawChs) {
%>
<div style="height:<%=collector.height%>px; width:100%;">
<table class="table"><%

  if (drawCand) {
    boolean someCandPresent = false;

%>
  <tr>
    <th colspan="2" class="tableHead"><%=collector.CANDIDATES_LABEL%></th>
  </tr>
</table>
<div style="height:<%=collector.height-16%>px;  width:100%; overflow:auto;">
<table width="100%" id="<%=collector.id+"_candidates"%>"><%

  if (ces != null && ces.size() > 0) {
    List keys = new ArrayList(ces.getEntryKeys());
    Collections.sort(keys, new ClientEntryComparator(ces));

    if (collector.customUrlToInclude == null) {

      //final SortedSet entryKeys = new TreeSet(ces.getEntryKeys());
      for (Iterator iterator = keys.iterator(); iterator.hasNext();) {
        String key = (String) iterator.next();
        if (key.indexOf(Collector.getCandidateFieldPrefix(id)) > -1) {
          String value = ces.getEntry(key).stringValue();
          someCandPresent = true;
          CheckBox check = new CheckBox("", Collector.FLD_CHOSEN_KEY + id + key.substring((Collector.FLD_CAND_HIDDEN_ID + id).length()), " ", "", collector.disabled, false, "", false);
          check.toolTip = key.toString();
          TextField textFieldId = new TextField("HIDDEN", "", key, "", 10, false);
          textFieldId.disabled = collector.disabled;
%>
  <tr class="alternate" >
    <td width="1%"><%check.toHtml(pageContext);%></td><td>
      <% if (collector.jspIncluderCandidates != null)
        collector.jspIncluderCandidates.toHtml(pageContext);%> <%=value%>
      <%textFieldId.toHtml(pageContext);%></td>
  </tr>
  <%
          }
        }
      } else {
        JspIncluderSupport jss = new JspIncluderSupport();
        pageState.addClientEntry("COLLECTOR_ID", collector.id);
        jss.urlToInclude = collector.customUrlToInclude;
        jss.toHtml(pageContext);
        someCandPresent = true;
      }
    }

    if (!someCandPresent) {
  %>
  <tr>
    <td colspan="<%=totColumns%>" align="center"><%=collector.NO_CANDIDATES%>
    </td>
  </tr>
  <%
    }
  } else if (drawChs) {

    boolean someChoPresent = false;
  %>
  <tr>
    <th colspan="2"class="tableHead"><%=collector.CHOSEN_LABEL%>
    </th>
  </tr>
</table>
<div style="height:<%=collector.height-16%>px;  width:100%; overflow:auto;">
  <table width="100%" id="<%=collector.id+"_chosen"%>"><%
        if ((collector.checkBoxes!=null) /*||  (collector.radioButtons!=null)*/ ){
          %><tr><th colspan="2" class="tableHead">&nbsp;</th><%
          }
        if (collector.checkBoxes!=null) {
          int col=1;
          for (Iterator iterator = collector.checkBoxes.keySet().iterator(); iterator.hasNext();) {
            String name = (String) iterator.next();
            %><th>
             <%

               CheckField toCheckAll = new CheckField("",Collector.FLD_CHECK_ADDITIONAL+col+"_ALL_"+collector.id,"" ,false);
               toCheckAll.script="onclick=\"var ck=this.checked;$(':checkbox[column="+col+"]').each(function(){this.checked = ck;});\"";
               toCheckAll.label = "";
               toCheckAll.toHtml(pageContext);%>
            </th><%
          col++;
          }%>
       </tr>
      <%}
    //additional column handling
    if ((collector.checkBoxes != null) ) {
  %>
<tr>
<th colspan="2" class="tableHead">&nbsp;</th><%

      if (collector.checkBoxes != null) {

        for (Iterator iterator = collector.checkBoxes.keySet().iterator(); iterator.hasNext();) {
          String name = (String) iterator.next();
            %><th><%=name%></th><%
        }
      }
    %></tr>
  <%}
    if (ces != null && ces.size() > 0) {
      List keys = new ArrayList(ces.getEntryKeys());
      Collections.sort(keys, new ClientEntryComparator(ces));

      for (Iterator iterator = keys.iterator(); iterator.hasNext();) {
        String key = (String) iterator.next();

        if (key.indexOf(Collector.getChosenFieldPrefix(id)) > -1) {
          String value = ces.getEntry(key).stringValue();
          someChoPresent = true;

          CheckBox check = new CheckBox("", Collector.getCandidateCheckValue(key), "", "", collector.disabled, false, "", false);
          check.toolTip = key.toString();
          TextField textFieldId = new TextField("HIDDEN", "", key, "", 10, false);
          textFieldId.disabled = collector.disabled;
  %>
  <tr class="alternate" >
    <td width="1%"><%check.toHtml(pageContext);%></td><td>
      <% if (collector.jspIncluderChosen != null)
        collector.jspIncluderChosen.toHtml(pageContext);%> <%=value%>
      <%textFieldId.toHtml(pageContext);%>
      </td>
    <%

      //additional column handling: checkBoxes
        int columPosition=0;
      if (collector.checkBoxes != null) {
        for (Iterator iteratorCb = collector.checkBoxes.keySet().iterator(); iteratorCb.hasNext();) {
          String name = (String) iteratorCb.next();
             final String adCbValue =Collector.getCandidateCheckAddPrefix(collector.id)+key.substring((Collector.FLD_CHS_HIDDEN_ID+id).length())+(String)collector.checkBoxes.get(name);
          CheckBox checkAd = new CheckBox("", adCbValue, "", "", collector.disabled, false, "", false);
          checkAd.script = "column='"+(++columPosition)+"'";
    %>
    <td align="center"><%checkAd.toHtml(pageContext);%></td>
    <%
        }
      }


    %></tr>
  <%
        }
      }
    }
    if (!someChoPresent) {
  %>
  <tr>
    <td colspan="<%=totColumns%>" align="center"><%=collector.NO_CHOSEN%>
    </td>
  </tr>
  <%
      }
      /* averacchi try resolving the bug about collector 14/06/2005 */
      request.setAttribute(Collector.ACTION, null);
    }
  %></table>
</div>
</div>
<%
  /*
  ________________________________________________________________________________________________________________________________________________________________________


  main parts

  ________________________________________________________________________________________________________________________________________________________________________

  */
} else {

%>
<table width="100%">
  <tr>
    <td valign="top" width="49%"><%

      request.setAttribute(Collector.ACTION, Collector.DRAW_CANDIDATES);
      collector.toHtml(pageContext);

    %></td>
    <td valign="middle" align="center"
        style="border-left:1px solid; border-right:1px solid;"><%

/*
________________________________________________________________________________________________________________________________________________________________________


COMMANDS

________________________________________________________________________________________________________________________________________________________________________

*/
    %>
      <table><%
        ButtonSubmit bs = new ButtonSubmit(form);
        bs.enabled = !collector.disabled;
        if (collector.MOVE_TO_SELECTED_LABEL != null) {
          bs.variationsFromForm.setCommand(Commands.MOVE_TO_SELECTED + id);
          bs.label = collector.MOVE_TO_SELECTED_LABEL;
          bs.toolTip = collector.MOVE_TO_SELECTED_TITLE;
          bs.width = "50";
%>
        <tr>
          <td align="center"><%bs.toHtml(pageContext);%></td>
          </tr>
        <%

          }
          if (collector.MOVE_ALL_TO_SELECTED_LABEL != null) {
            bs.label = collector.MOVE_ALL_TO_SELECTED_LABEL;
            bs.toolTip = collector.MOVE_ALL_TO_SELECTED_TITLE;
            bs.variationsFromForm.setCommand(Commands.MOVE_ALL_TO_SELECTED + id);
        %>
        <tr>
          <td align="center"><%bs.toHtml(pageContext);%></td>
          </tr>
        <%

          }
          if (collector.SYNCHRONIZE_LABEL != null) {
            bs.variationsFromForm.setCommand(Commands.SYNCHRONIZE + id);
            bs.label = collector.SYNCHRONIZE_LABEL;
            bs.toolTip = collector.SYNCHRONIZE_TITLE;

        %>
        <tr>
          <td align="center"><%bs.toHtml(pageContext);%></td>
          <%
            }
            if (collector.MOVE_ALL_TO_UNSELECTED_LABEL != null) {
              bs.variationsFromForm.setCommand(Commands.MOVE_ALL_TO_UNSELECTED + id);
              bs.label = collector.MOVE_ALL_TO_UNSELECTED_LABEL;
              bs.toolTip = collector.MOVE_ALL_TO_UNSELECTED_TITLE;
              %>
          <tr>
            <td align="center"><%bs.toHtml(pageContext);%></td>
           </tr>
          <%

            }
            if (collector.MOVE_TO_UNSELECTED_LABEL != null) {
              bs.variationsFromForm.setCommand(Commands.MOVE_TO_UNSELECTED + id);
              bs.label = collector.MOVE_TO_UNSELECTED_LABEL;
              bs.toolTip = collector.MOVE_TO_UNSELECTED_TITLE;

          %>
          <tr>
            <td align="center" width="2%"><%bs.toHtml(pageContext);%></td>
            </tr>
          <%

            }
          %></tr>
      </table>
      </td>
    <td valign="top" width="49%"><%


      request.setAttribute(Collector.ACTION, Collector.DRAW_CHOSEN);
      collector.toHtml(pageContext);

    %></td>
  </tr>
</table>
<%
  }
%>