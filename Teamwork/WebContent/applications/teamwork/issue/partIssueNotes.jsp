<%@ page import="org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.input.DateField, org.jblooming.waf.html.input.TextArea, org.jblooming.waf.html.input.TextField, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState" %>
<%-- ------------------------------------------------------------- NOTES FOR STATUS CHANGE INSERTION START --- è posizionato fisso sul top e ce n'è uno solo ------------------------------------------------------------- --%>
<div id="insertStatusNotes" class="notesEditor">
<%
  TextArea ta= new TextArea(I18n.get("ADD_COMMENT"),"HIS_NOTES","<br>",50,5,"style='height:125px;width:100%'");

  ta.toHtml(pageContext);

  ButtonJS savenotes=new ButtonJS("saveStatusNotes($(this))");
  savenotes.label=I18n.get("SAVE");
  savenotes.additionalCssClass = "button noprint first small";
  savenotes.toHtml(pageContext);
%>
  <span class="teamworkIcon issueNotesClose" title="hide" onclick="$(this).closest('.notesEditor').fadeOut();" style="cursor: pointer">x</span>

</div>


<%-- ------------------------------------------------------------- COMMENT INSERTION START ----------- è clonato e inserito dopo le righe della history ----------------------------------------------------- --%>
<div id="insertComment" style="display:none" class="notesEditor">
  <%
    ta= new TextArea("","HIS_COMMENT","",50,2,"");
    ta.script="placeHolder='" + I18n.get("ADD_COMMENT") + "'";
    ta.setAutosize(125,400,15);
    ta.maxlength=0;
    ta.toHtml(pageContext);
  %><div style="margin-top: 10px"><%
    savenotes=new ButtonJS("saveComment($(this))");
    savenotes.label=I18n.get("ADD");
    savenotes.additionalCssClass="first small";
    savenotes.toHtml(pageContext);
  %></div>
</div>
