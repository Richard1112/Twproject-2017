<%if (false) {%><script><%}%>

/**
*
* @param request {assId:123,costId:23,date: new Date()}
 */
function openExpenseEditorPopup(el, request, callback) {

  if (!request.assId)
    return;

  var url = contextPath + "/applications/teamwork/task/financial/expenseEditor.jsp?";

  url+="&assId="+request.assId;


  if (!request.costId && !(request.date && typeof (date)=="object" )){
    request.date= new Date();
  }

  if (request.date)
    url+="&creationDate="+request.date.format();

  if (request.costId)
    url+="&costId="+request.costId;


  openBlackPopup(url, 500, 500,callback);
}

  <%if (false) {%></script><%}%>
