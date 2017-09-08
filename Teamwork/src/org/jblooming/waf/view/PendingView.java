package org.jblooming.waf.view;


/**
 *
 */
public class PendingView {

  public static final String package_prefix = "com.twproject.teamwork.waf.view";
  public static final String PENDINGVIEW = package_prefix + ".PENDINGVIEW";

  /**
   * uniquely identifier; needed as same view may belong to different sequences of views
   */
  public String id;
  /**
   * when such FLD_URL is reached, the Performer redirects to pendingView
   */
  public String trigger;
  public PageState pendingView;

  private PendingView() {
  }

  public PendingView(String id, String trigger, PageState pendingView) {
    this.id = id;
    this.trigger = trigger;
    this.pendingView = pendingView;
  }

}
