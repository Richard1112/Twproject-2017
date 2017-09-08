package org.jblooming.waf.html.core;

/**
 * JavaScript Templating
 */
public class JST {
  public static String start(String type){
    return "<div class=\"__template__\" type=\""+type+"\"><!--";
  }

  public static String end() {
    return "--></div>";
  }
}


