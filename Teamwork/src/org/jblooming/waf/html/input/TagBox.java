package org.jblooming.waf.html.input;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.classification.Taggable;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.security.Area;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.view.PageState;

import javax.servlet.jsp.PageContext;
import java.io.IOException;

public class TagBox extends TextField {

  private Class taggableClass;


  public static final String DRAW_INPUT="DI";
  public Area area;
  public int maxResult=20;
  public String jspFillerName = "/commons/layout/tagBox/tagBoxFiller.jsp";

  public String tagPropertyName;
  public boolean autostart=false; // when false drop-down is opened only after typing first char. Wher true it starts when entering the field

  public TagBox(String fieldName,  Class<? extends Taggable>  taggableClassWithArea, Area area) {
    super(fieldName,"&nbsp;");
    this.taggableClass=taggableClassWithArea;
    this.area=area;
    this.tagPropertyName="tags";

  }

  public TagBox(String fieldName,  Class<? extends Taggable>  taggableClass) {
    this (fieldName,taggableClass,null);
  }

  public TagBox(String fieldName,  Class<? extends IdentifiableSupport>  aPersistentClass, String tagPropertyName) {
    super(fieldName,"&nbsp;");
    this.taggableClass=aPersistentClass;
    this.tagPropertyName=tagPropertyName;
  }


  public String getDiscriminator() {
    return TagBox.class.getName();
  }


  public void init(PageContext pageContext) throws IOException {
    PageState ps = PageState.getCurrentPageState(pageContext);
    if (!ps.initedElements.contains(getDiscriminator())) {
      pageContext.getOut().print("<script>$(function(){initialize(contextPath+\"/commons/layout/tagBox/tagBox.js\",\"script\")});</script>");
      pageContext.getOut().print("<script>document.jspFillerName = '"+jspFillerName+"' ;</script>");
      ps.initedElements.add(getDiscriminator());
    }
  }

  public void toHtml(PageContext pageContext) {
    try {
      init(pageContext);
    } catch (Throwable e) {
      throw new PlatformRuntimeException();
    }

   script= JSP.w(script)+" autocomplete='off' taggableClass=\""+taggableClass.getName()+"\" maxResult=\""+maxResult+"\" tagPropertyName=\""+tagPropertyName+"\""+
           (area==null?"": " areaId=\""+area.getId()+"\"")+
           (autostart?" autoStart=1":"");
   this.fieldClass=this.fieldClass+" tagBox";
    super.toHtml(pageContext);
  }


}