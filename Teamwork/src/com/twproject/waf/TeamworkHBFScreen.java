package com.twproject.waf;

import org.jblooming.waf.ScreenArea;
import org.jblooming.waf.ScreenBasic;

import javax.servlet.jsp.PageContext;

public class TeamworkHBFScreen extends ScreenBasic {

  public TeamworkHBFScreen() {
    super();
  }

  public TeamworkHBFScreen(ScreenArea body) {

    super(body);
    initialize(body);
  }


  public void initialize(ScreenArea body) {
    setBody(body);
    urlToInclude = "/applications/teamwork/screens/teamworkScreen.jsp";
    menu = new ScreenArea("/applications/teamwork/parts/teamworkMenu.jsp");
  }

}
