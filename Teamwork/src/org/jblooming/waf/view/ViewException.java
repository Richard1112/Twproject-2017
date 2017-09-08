package org.jblooming.waf.view;

/**
 * Date: 24-gen-2003
 * Time: 9.24.42
 *
 * @author Pietro Polsinelli dev@open-lab.com
 */
public class ViewException extends Exception {

  public ViewException(String s) {
    super(s);
  }

  public ViewException(Exception e) {
    super(e);
  }

  public ViewException(String s, Exception e) {
    super(s, e);
  }
}
