package com.opnlb.website.content;

import org.jblooming.operator.Operator;
import org.jblooming.ontology.LoggableIdentifiableSupport;
import org.jblooming.ontology.SerializedMap;
import com.opnlb.website.template.Template;
import com.opnlb.website.portlet.Portlet;
import com.opnlb.website.page.WebSitePage;

import java.util.Comparator;

/**
 * Content (c) 2005 - Open Lab - www.open-lab.com
 */
public class Content extends LoggableIdentifiableSupport  {

  private Operator operator;
  //private Template template;
  //private boolean defaultTemplate = true;
  private Portlet portlet;
  private WebSitePage page;

  // l'area del template è diventata un filtro
  private String area;
  private int order = 0;
  // attribute peculiar to site admin: if no user personal choice this is the default page composer
  private boolean defaultConfiguration = false;

  private SerializedMap<String, String> portletParams = new SerializedMap();


  // if checkbox selected content is propagated to all children
  private boolean globalAssociation = false;


  public Content () {
  }

  public Operator getOperator() {
    return operator;
  }

  public void setOperator(Operator operator) {
    this.operator = operator;
  }
//
//  public Template getTemplate() {
//    return template;
//  }
//
//  public void setTemplate(Template template) {
//    this.template = template;
//  }

  public WebSitePage getPage() {
    return page;
  }

  public void setPage(WebSitePage page) {
    this.page = page;
  }

  public String getArea() {
    return area;
  }

  public void setArea(String area) {
    this.area = area;
  }

  public Portlet getPortlet() {
    return portlet;
  }

  public void setPortlet(Portlet portlet) {
    this.portlet = portlet;
  }

  public int getOrder() {
    return order;
  }

  public void setOrder(int order) {
    this.order = order;
  }

  public boolean getDefaultConfiguration() {
    return defaultConfiguration;
  }

  public void setDefaultConfiguration(boolean defaultConfiguration) {
    this.defaultConfiguration = defaultConfiguration;
  }

  public boolean isGlobalAssociation() {
    return globalAssociation;
  }

  public void setGlobalAssociation(boolean globalAssociation) {
    this.globalAssociation = globalAssociation;
  }

  public SerializedMap<String, String> getPortletParams() {
    return portletParams;
  }

  public void setPortletParams(SerializedMap<String, String> portletParam) {
    this.portletParams = portletParam;
  }


}
