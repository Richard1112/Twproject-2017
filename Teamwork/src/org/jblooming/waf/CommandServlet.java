package org.jblooming.waf;

import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.*;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;

import javax.servlet.*;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 10-dic-2007 : 13.16.45
 */
public class CommandServlet extends HttpServlet {

  protected void doGet (HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse) throws javax.servlet.ServletException, java.io.IOException {
    doMe(httpServletRequest,httpServletResponse);
    }

  protected void doPost(HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse) throws javax.servlet.ServletException, java.io.IOException {
    doMe(httpServletRequest,httpServletResponse);
  }

  private void doMe(HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse){
    try {
      ApplicationState.commandController.newInstance().perform(httpServletRequest, httpServletResponse);
    } catch (Exception e) {
      throw new PlatformRuntimeException(e);
    }

  }

}
