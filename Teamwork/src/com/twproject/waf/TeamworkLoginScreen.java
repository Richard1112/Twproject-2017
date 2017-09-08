package com.twproject.waf;

import org.jblooming.waf.ScreenArea;
import org.jblooming.waf.ScreenBasic;

public class TeamworkLoginScreen extends ScreenBasic {

   public TeamworkLoginScreen() {
    super();
  }




  public TeamworkLoginScreen(ScreenArea body) {
    initialize(body);
  }

  public void initialize(ScreenArea body) {
    setBody(body);
    urlToInclude = "/applications/teamwork/screens/teamworkLoginScreen.jsp";

  }
}
