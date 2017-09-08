package org.jblooming.waf.html.container;

import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.core.JspIncluder;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.ServletException;
import javax.servlet.jsp.PageContext;
import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedList;

public class TabSet extends JspHelper implements HtmlBootstrap {

  public static final String init = TabSet.class.getName();

  boolean openTabBarCalled;
  boolean closeTabBarCalled;

  public String style = "";

  public LinkedList<JspIncluder> pre =new LinkedList<JspIncluder>();
  public LinkedList<Tab> tabs =new LinkedList<Tab>();
  public LinkedList<JspIncluder> post =new LinkedList<JspIncluder>();

  public final static String BAR = "TABSET_BAR";
  public final static String END = "TABSET_END";

  public TabSet(String id, PageState pageState) {
    this.id = id;
    this.urlToInclude = "/commons/layout/tabSet/partTabSet.jsp";
    // si mette sul pageState così si ripiglia
    pageState.attributes.put("_CURTABSET",this);
  }

  public TabSet getCurrentTabSet(PageState pageState) {
    return (TabSet) pageState.attributes.get("_CURTABSET");
  }


  public void drawBar(PageContext pageContext) {
    pageContext.getRequest().setAttribute(Commands.COMMAND, BAR);
    toHtml(pageContext);
    openTabBarCalled = true;
  }

  public void end(PageContext pageContext) {
    pageContext.getRequest().setAttribute(Commands.COMMAND, END);
    toHtml(pageContext);
    closeTabBarCalled = true;
  }


  public void addTab(Tab tab) {
    tab.tabSet = this;
    tab.urlToInclude = this.urlToInclude;
    tabs.add(tab);
  }

  public Tab getTab(String tabId){
    for (Tab tab: tabs){
      if (tab.id.equals(tabId))
        return tab;
    }
    return null;
  }

  public String getDiscriminator() {
    return id;
  }

  public boolean validate(PageState pageState) throws IOException, ServletException {
    boolean result = false;
    for (Iterator iterator = tabs.iterator(); iterator.hasNext();) {
      Tab tab = (Tab) iterator.next();
      result = (!tab.openTabCalled && !tab.closeTabCalled) || (tab.openTabCalled && tab.closeTabCalled);
      if (!result)
        break;
    }

    return result && openTabBarCalled && closeTabBarCalled;
  }

  public static void pointToTab(String tabSetId, String tabId, PageSeed pointer) {
    pointer.addClientEntry(tabSetId,tabId);
  }

}