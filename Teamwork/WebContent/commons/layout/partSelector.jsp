<%@ page import ="org.jblooming.waf.html.core.JspIncluderSupport,
                  org.jblooming.waf.html.input.CheckBox,
                  org.jblooming.waf.html.input.RadioButton,
                  org.jblooming.waf.html.input.Selector,
                  org.jblooming.waf.html.input.TextField,
                  org.jblooming.waf.view.ClientEntries,
                  org.jblooming.ontology.PlatformComparators.ClientEntryComparator,
                  org.jblooming.waf.view.PageState,
                  java.util.ArrayList,
                  java.util.Collections,
                  java.util.Iterator,
                  java.util.List"%>
<%

  PageState pageState = PageState.getCurrentPageState(request);

    final boolean drawSelected = Selector.DRAW_SELECTED.equals(request.getAttribute(Selector.ACTION));
    final boolean drawTheRest = Selector.DRAW_THEREST.equals(request.getAttribute(Selector.ACTION));

    Selector selector = (Selector)JspIncluderSupport.getCurrentInstance(request);
    ClientEntries ces = pageState.getClientEntries();


  if (! drawSelected && !drawTheRest) {

    %><div style="<%=selector.height==null ? "" : "height:"+selector.height+";"%> overflow:auto;">
    <table class="table" border="0"><%


  %><tr><th class="tableHead first"><%=selector.label%></th><%
  //additional column handling
    if (selector.checkBoxes!=null) {
        for (Iterator iterator = selector.checkBoxes.keySet().iterator(); iterator.hasNext();) {
        String name = (String) iterator.next();
        %><th class="tableHead"><%=name%></th><%
        }
    }
    if (selector.radioButtons!=null) {
        for (Iterator iterator = selector.radioButtons.keySet().iterator(); iterator.hasNext();) {
          String name = (String) iterator.next();
          %><th class="tableHead"><%=name%></th><%
        }
    }
  %></tr><%

  if (ces!=null && ces.size()>0) {
    if (selector.selectedOnTop){
      request.setAttribute(Selector.ACTION,Selector.DRAW_SELECTED);
      selector.toHtml(pageContext);
    }
    request.setAttribute(Selector.ACTION,Selector.DRAW_THEREST);
    selector.toHtml(pageContext);    
  }
  request.setAttribute(Selector.ACTION,"");

    %></table></div>

<script>
  function synch_<%=selector.id%>(selector, rowId){
    var cb;
    <%
    if (selector.checkBoxes!=null) {
       for (Iterator iteratorCb = selector.checkBoxes.keySet().iterator(); iteratorCb.hasNext();) {
        String name = (String) iteratorCb.next();
       %>resetCB(obj("<%=Selector.FLD_CHECK_ADDITIONAL+selector.id%>"+rowId+<%=(String)selector.checkBoxes.get(name)%>),selector.checked);<%
      }
    }
    if (selector.radioButtons!=null) {
       int i=0;
       for (Iterator iteratorRb = selector.radioButtons.keySet().iterator(); iteratorRb.hasNext() ; iteratorRb.next()) {
        i++;
         %>resetCB(obj("<%=Selector.FLD_RADIO_ADDITIONAL+selector.id%>"+rowId+"<%=i%>"),selector.checked);<%
      }
    }
    %>
}

function resetCB(cb,sel){
  if (!sel){
    cb.disabled=true;
    cb.checked=false;
  } else {
    cb.disabled=false;
  }
}
</script><%

/*
________________________________________________________________________________________________________________________________________________________________________


draw rows

________________________________________________________________________________________________________________________________________________________________________

*/
} else {

  if(ces != null && ces.size()>0) {

    List keys = new ArrayList(ces.getEntryKeys());
    if (!selector.orderById)
      Collections.sort(keys, new ClientEntryComparator(ces));

    for (Iterator iterator = keys.iterator(); iterator.hasNext();) {
      String key = (String) iterator.next();
      String checkBoxName = Selector.getCheckBoxName(key);
      String checkBoxValue = pageState.getEntry(checkBoxName).stringValueNullIfEmpty();

      /**
       * 30/11/2005
       * teoros changed all CheckBoxes with CheckFields
       * see also: Selector.java
       */
      if (key.indexOf(Selector.getHiddenPrefix(selector.id))>-1 && ( !selector.selectedOnTop  ||(drawSelected ? checkBoxValue!=null : checkBoxValue==null))) {

        String value = ces.getEntry(key).stringValue();
        String rowId= key.substring((Selector.FLD_HIDDEN_ID+selector.id).length());

        CheckBox check = new CheckBox(value, checkBoxName, "&nbsp;", "", selector.disabled, false, "", false);
        //CheckField check = new CheckField(checkBoxName, "&nbsp;", false);
        check.label = value;
        check.toolTip = key;
        check.script="onClick=\"synch_"+selector.id+"(this,'"+ rowId +"');\"";
        TextField textFieldId = new TextField("hidden","",key,"",10,false);
        textFieldId.disabled = selector.disabled;

            %><tr class="alternate" >
                <td><%check.toHtml(pageContext);%><%textFieldId.toHtml(pageContext);%></td><%

            boolean disabled=checkBoxValue==null;

            //additional column handling: checkBoxes
            if (selector.checkBoxes!=null) {
               for (Iterator iteratorCb = selector.checkBoxes.keySet().iterator(); iteratorCb.hasNext();) {
                String name = (String) iteratorCb.next();
                 final String adCbValue = Selector.FLD_CHECK_ADDITIONAL+selector.id+rowId+(String)selector.checkBoxes.get(name);
                 CheckBox checkAd = new CheckBox("", adCbValue, "", "", disabled, disabled, "", false);
                 //CheckField checkAd = new CheckField(adCbValue, "", false);
                 //checkAd.enabled = !disabled;

                %><td align="center"><%checkAd.toHtml(pageContext);%></td><%
              }
            }

            //additional column handling: radioButtons
            if (selector.radioButtons!=null) {
               int j=0;
               for (Iterator iteratorCb = selector.radioButtons.keySet().iterator(); iteratorCb.hasNext();) {
                 j++;
                 String name = (String) iteratorCb.next();
                 String element = key.substring((Selector.FLD_HIDDEN_ID+selector.id).length());
                 RadioButton radioBt = new RadioButton("", Selector.FLD_RADIO_ADDITIONAL+selector.id+element, "", "", "",disabled, "");
                 radioBt.id=radioBt.fieldName+j;
                 radioBt.buttonValue=selector.radioButtons.get(name).toString();

                %><td align="center"><%radioBt.toHtml(pageContext);%></td><%
                }
            }
            %></tr><%
      }
    }
  }
}
%>