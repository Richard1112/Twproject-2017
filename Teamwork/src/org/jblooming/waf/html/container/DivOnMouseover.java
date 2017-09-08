package org.jblooming.waf.html.container;

import org.jblooming.utilities.JSP;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.core.JspIncluder;
import org.jblooming.waf.html.core.TextEmbedder;
import org.jblooming.waf.view.PageState;

import javax.servlet.jsp.PageContext;
import java.util.LinkedList;
import java.util.List;

public class DivOnMouseover extends JspHelper {

    public List<JspIncluder> buttonList = new LinkedList<JspIncluder>();
    public JspIncluder content;
    public JspIncluder opener;

    public String additionalDropDownClass;
    public boolean openerInTextOnly = false;
    public boolean arrowOpener = false;

    public static final String DRAW = "DRAW";

    public DivOnMouseover(JspIncluder rolloverElement) {
        this(null,rolloverElement);
    }

    public DivOnMouseover(JspIncluder content, JspIncluder rolloverElement) {this(content,rolloverElement,false);}

    public DivOnMouseover(JspIncluder content, JspIncluder rolloverElement, boolean arrowOpener) {
        this.content = content;
        this.opener = rolloverElement;
        this.arrowOpener = arrowOpener;
        this.urlToInclude = "/commons/layout/partDivOnMouseover.jsp";
    }


    public void addSeparator() {
        buttonList.add(new TextEmbedder("<hr>"));
    }

    public void addTextSeparator(String text) {
        buttonList.add(new TextEmbedder(text));
    }

    public void addButton(JspIncluder element) {
        buttonList.add(element);
    }

    public void addButtons(List<? extends JspHelper> bs) {
      if (JSP.ex(bs))
        buttonList.addAll(bs);
    }

    public void toHtml(PageContext pc) {
        if (content!=null ||buttonList.size()>0 ){
            pc.getRequest().setAttribute(ACTION, DRAW);
            super.toHtml(pc);
        }
    }

    public void toHtmlInTextOnlyModality(PageContext pc) {
        openerInTextOnly = true;
        toHtml(pc);
    }

}
