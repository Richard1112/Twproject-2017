package org.jblooming;

import org.jblooming.utilities.DateUtilities;

import java.util.Date;

/**
 * Date: 16-dic-2002
 * Time: 16.42.44
 *
 * @author Pietro Polsinelli dev@open-lab.com
 */
public class ApplicationException extends Exception {

  public ApplicationException() {
    super();
  }

  public ApplicationException(String s) {
    super(s);
  }

  public ApplicationException(Exception e) {
    super(e);
  }

  public ApplicationException(String s, Exception e) {
    super(s, e);
  }
}
