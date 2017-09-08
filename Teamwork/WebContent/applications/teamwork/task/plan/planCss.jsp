<%@ page import="org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState" %><%
  PageState pageState = PageState.getCurrentPageState(request);

%>
<style type="text/css">

  .cell {
    font-size: 12px;
  }

  .cell:focus{
    outline: none;
    box-shadow: none;
  }

  .day {
    vertical-align: middle;
  }

  .overPlanned{
    background-color: #f79705;
    color:#fff;
  }

  .underplanned{
  <%if (I18n.isActive("CUSTOM_FEATURE_SHOW_UNDERPLANNED")){%>
    background-color: #ffff00;
  <%}%>
  }


  <%-- DO NOT CHANGE ORDER: IT IS VERY !IMPORTANT ------------------------ START -----------------------%>
  .routine{
    background-color:#d0d0d0;
  }

  .notAvailable {
      background-color:#efbfc4;
  }

  .outOfScope{
      background-color: rgba(255, 204, 51, 0.50);
      cursor:no-drop;
  }

  .exceeded{
    background-color:rgba(239, 74, 69, 0.80);
  }


  .outOfScope input{
      cursor:no-drop;
  }

  .focused {
      background-color: #BCFF3A;
  }

  .highlightPeriod {
      background-color: #ffff60;
  }




  <%-- DO NOT CHANGE ORDER: IT IS VERY !IMPORTANT ------------------------ END ------------------------%>

  .noteEditorButton{
    display:none;
    width: 0;
    height: 0;
    position: absolute;
    top:0;
    right: 0;
    border-style: solid;
    border-width: 0 10px 10px 0;
    border-color: transparent #e0e0e0 transparent transparent;
  }

  .hasPlan .noteEditorButton{
    display: inline-block;
  }


  .hasNotes .noteEditorButton{
    border-color: transparent #a00000 transparent transparent;
  }



  .zero_panned input{
    color: #ddd;
  }


  .showHead{
    background-color:#F9EFC5;
    color: #000 !important;
  }

  .showHead .button{
    color:#000;
  }

  /*    .total{
          font-size:11px;
      }*/

  .assDisabled{
    opacity:.6;
  }

  .estimOnPlanned{
    font-size:10px;
    font-family: arial, helvetica, sans-serif;
  }

  .estimOnPlanned input{
    width: 35px;
    background: rgba(0,0,0,0.1);
    border: none;
    text-align: right;
    font-size:10px;
  }


  .microEdit {
    border: 1px solid #B1AFAF;
    background-color: #FFFFFF;
    width: 250px;
    min-height: 40px;
    position: absolute;
    z-index: 10;
    padding: 10px;
    margin-top: 10px;
    border-radius: 4px;
    box-shadow: 0 0 4px rgba(0,0,0,0.4);
  }

  .notesInEdit{
    /*box-shadow: 0 0 8px #777;*/
  }

  table.edged > tbody > tr > td {
    padding: 0 2px;
  }

  [disabled], [readonly] {
    background-color: transparent;
    color: #a5a5a5;
  }

  .planResWL .tdCell input {
      width: 100%;
      height: 100%;
      padding:6px 0;
      text-align: right;
  }

/*
  .planTaskname{
      max-width: 300px;
      overflow: hidden;
  }
*/

  .columnTaskName {
    width: 40%;
  }

/*
  .columnTaskName a {
    max-width: 270px;
  }
*/

/*
  .planTaskname .button.textual {
      margin-top: -2px;
      margin-bottom: -2px;
  }

*/
  .planResWL .totalRow {
      font-size: 80%;

  }

  .planResWL .totalRow td.day {
      font-family: arial, helvetica, sans-serif;
  }



  table.edged.planResWL  > tbody > tr > td{
      border-bottom: 1px solid rgba(0, 0, 0, 0.25);
      border-right: 1px solid rgba(0, 0, 0, 0.25);
  }

  table.edged.planResWL  > tbody > tr > td:last-of-type, table.edged.planResWL > tbody > tr > td.columnTaskName {
      border-right: none;
  }



</style>
