package org.jblooming.flowork.waf;

import org.jblooming.waf.ScreenArea;
import org.jblooming.waf.ScreenBasic;

public class FloworkPopUpScreen extends ScreenBasic {

   public FloworkPopUpScreen() {
    super();
  }

  public FloworkPopUpScreen(ScreenArea body) {
    initialize(body);
  }

  public void initialize(ScreenArea body) {
    setBody(body);
    urlToInclude = "/commons/flowork/screens/floworkPopUpScreen.jsp";
    
  }
}
