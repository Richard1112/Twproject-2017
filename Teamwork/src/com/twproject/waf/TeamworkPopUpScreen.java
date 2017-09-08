package com.twproject.waf;

import org.jblooming.waf.ScreenArea;
import org.jblooming.waf.ScreenBasic;
import org.jblooming.waf.view.PageState;

public class TeamworkPopUpScreen extends ScreenBasic {

   public TeamworkPopUpScreen() {
    super();
  }

  public TeamworkPopUpScreen(ScreenArea body) {
    initialize(body);
  }

  public void initialize(ScreenArea body) {
    setBody(body);
    urlToInclude = "/applications/teamwork/screens/teamworkPopupScreen.jsp";
  }

   public void register(PageState pageState) {
     super.register(pageState);
     pageState.setPopup(true);
   }


}
