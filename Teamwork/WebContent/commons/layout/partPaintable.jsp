<%@ page import="org.jblooming.utilities.JSP,
                 org.jblooming.waf.SessionState,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.display.paintable.*,
                 org.jblooming.waf.view.PageState, java.io.IOException" %><%!


  public void mkDiv(int x,int y,int w,int h, String color,PageContext pageContext) throws IOException {
    pageContext.getOut().print("<div style=\"position:absolute; left:" + x + "px; top:" + y + "px; width:" + w + "px; height:" + h + "px; clip:rect(0," + w + "px," + h + "px,0);" +
                "background-color:" + color +";overflow:hidden" + ";\"></div>");
            //(!jg_moz? ";overflow:hidden" : "") +
  }



  public void  drawLine(int x1, int y1, int x2, int y2, String color,PageContext pageContext) throws IOException {
    if (x1 > x2) {
      int _x2 = x2;
      int _y2 = y2;
      x2 = x1;
      y2 = y1;
      x1 = _x2;
      y1 = _y2;
    }
    int dx = x2 - x1, dy = Math.abs(y2 - y1),
            x = x1, y = y1,
            yIncr = (y1 > y2)? -1 : 1;

    if (dx >= dy) {
      int pr = dy << 1,
              pru = pr - (dx << 1),
              p = pr - dx,
              ox = x;
      while ((dx--) > 0) {
        ++x;
        if (p > 0) {
          mkDiv(ox, y, x - ox, 1,  color, pageContext);
          y += yIncr;
          p += pru;
          ox = x;
        }
        else p += pr;
      }
      mkDiv(ox, y, x2 - ox + 1, 1,  color, pageContext);
    } else {
      int pr = dx << 1,
              pru = pr - (dy << 1),
              p = pr - dy,
              oy = y;
      if (y2 <= y1) {
        while ((dy--) > 0) {
          if (p > 0) {
            mkDiv(x++, y, 1, oy - y + 1,  color, pageContext);
            y += yIncr;
            p += pru;
            oy = y;
          } else {
            y += yIncr;
            p += pr;
          }
        }
        mkDiv(x2, y2, 1, oy - y2 + 1,  color, pageContext);
      }
      else {
        while ((dy--) > 0) {
          y += yIncr;
          if (p > 0) {
            mkDiv(x++, oy, 1, y - oy,  color, pageContext);
            p += pru;
            oy = y;
          } else
            p += pr;
        }
        mkDiv(x2, oy, 1, y2 - oy + 1,  color, pageContext);
      }
    }
  }


  public void drawLine2d(int x1, int y1, int x2, int y2, int linesize, String color,PageContext pageContext) throws IOException {
    int _s;
    if (x1 > x2) {
      int _x2 = x2;
      int _y2 = y2;
      x2 = x1;
      y2 = y1;
      x1 = _x2;
      y1 = _y2;
    }
    int dx = x2 - x1, dy = Math.abs(y2 - y1),
            x = x1, y = y1,
            yIncr = (y1 > y2)? -1 : 1;

    if (dx >= dy) {
      if (dx > 0 && linesize - 3 > 0) {
        _s = (int) (linesize * dx * Math.sqrt(1 + dy * dy / (dx * dx)) - dx - (linesize >> 1) * dy) / dx;
        _s = (int)((linesize - 4)!=0? Math.ceil(_s) : Math.round(_s)) + 1;
        //_s = (!(linesize - 4)? Math.ceil(_s) : Math.round(_s)) + 1;
      } else
         _s = linesize;
      int ad =(int) Math.ceil(linesize / 2);

      int pr = dy << 1,
              pru = pr - (dx << 1),
              p = pr - dx,
              ox = x;
      while ((dx--) > 0) {
        ++x;
        if (p > 0) {
          mkDiv(ox, y, x - ox + ad, _s,  color, pageContext);
          y += yIncr;
          p += pru;
          ox = x;
        } else
          p += pr;
      }
      mkDiv(ox, y, x2 - ox + ad + 1, _s,  color, pageContext);
    } else {
      if (linesize - 3 > 0) {
        _s = (int) (linesize * dy * Math.sqrt(1 + dx * dx / (dy * dy)) - (linesize >> 1) * dx - dy) / dy;
        _s = (int)((linesize - 4)!=0? Math.ceil(_s) : Math.round(_s)) + 1;
      } else
       _s = linesize;
      int ad = Math.round(linesize / 2);

      int pr = dx << 1;
              int pru = pr - (dy << 1);
              int p = pr - dy;
              int oy = y;
      if (y2 <= y1) {
        ++ad;
        while ((dy--) > 0) {
          if (p > 0) {
            mkDiv(x++, y, _s, oy - y + ad,  color, pageContext);
            y += yIncr;
            p += pru;
            oy = y;
          } else {
            y += yIncr;
            p += pr;
          }
        }
        mkDiv(x2, y2, _s, oy - y2 + ad,  color, pageContext);
      } else {
        while ((dy--) > 0) {
          y += yIncr;
          if (p > 0) {
            mkDiv(x++, oy, _s, y - oy + ad,  color, pageContext);
            p += pru;
            oy = y;
          } else
            p += pr;
        }
        mkDiv(x2, oy, _s, y2 - oy + ad + 1,  color, pageContext);
      }
    }
  }


  public void  drawLineDot(int x1, int y1, int x2, int y2, String color,PageContext pageContext) throws IOException {
    if (x1 > x2) {
      int _x2 = x2;
      int _y2 = y2;
      x2 = x1;
      y2 = y1;
      x1 = _x2;
      y1 = _y2;
    }
    int dx = x2 - x1;
    int dy = Math.abs(y2 - y1);
    int x = x1;
    int y = y1;
            int yIncr = (y1 > y2)? -1 : 1;
            boolean drw = true;
    if (dx >= dy) {
      int pr = dy << 1,
              pru = pr - (dx << 1),
              p = pr - dx;
      while ((dx--) > 0) {
        if (drw)
          mkDiv(x, y, 1, 1,  color, pageContext);
        drw = !drw;
        if (p > 0) {
          y += yIncr;
          p += pru;
        } else
          p += pr;
        ++x;
      }
      if (drw)
        mkDiv(x, y, 1, 1,  color, pageContext);
    } else {
      int pr = dx << 1,
              pru = pr - (dy << 1),
              p = pr - dy;
      while ((dy--) > 0) {
        if (drw) this.mkDiv(x, y, 1, 1,  color, pageContext);
        drw = !drw;
        y += yIncr;
        if (p > 0) {
          ++x;
          p += pru;
        } else
          p += pr;
      }
      if (drw)
        mkDiv(x, y, 1, 1,  color, pageContext);
    }
  }

%><%

  Paintable pasu = (Paintable) JspIncluderSupport.getCurrentInstance(request);

  String additionalOnClickScript = pasu.additionalOnClickScript == null ? "" : ("onClick=\"" + pasu.additionalOnClickScript + "\"");
  String id = "id=\"" + pasu.id + "\"";
  String toolTip = pasu.generateToolTip();

  String script = (pasu.script == null ? "" : pasu.script);

  String cursorStyle = additionalOnClickScript.trim().length() > 0 ? "cursor:pointer;" : "";

  if (Paintable.DRAW_FOLIO.equals(request.getAttribute(Paintable.ACTION))) {
    Folio folio = (Folio) JspIncluderSupport.getCurrentInstance(request);

    // skifid trick to force stupid browser to display correctly things
    int w = folio.pixelWidth;
    int h = folio.pixelHeight;
    if (folio.lineSize > 0 ){
      w = w - folio.lineSize * 2;
      h = h - folio.lineSize * 2;
    }

    String finalStyle="";
    finalStyle = "position:relative; left:" + folio.pixelLeft + "px; top:" + folio.pixelTop + "px; width:" + w + "px; height:" + h + "px; overflow:" + (folio.showScroll ? "auto;" : "hidden;");
    if (folio.style != null)
      finalStyle = finalStyle + folio.style;
    else
      finalStyle = finalStyle + "border:" + (folio.lineSize == 0 ? "" : folio.lineSize+"") + "px " + (folio.lineStyle == null ? "" : folio.lineStyle) + " " + (folio.color == null ? "" : folio.color) + ";";

%> <div class=" canvas folio" <%=id%> style="<%=finalStyle%><%=cursorStyle%>" <%=script%> <%=additionalOnClickScript%>  <%=toolTip%> realHeight="<%=folio.height%>" realWidth="<%=folio.width%>" realLeft="<%=folio.left%>" realTop="<%=folio.top%>"><%
  for (Paintable paintable : folio.paintables) {
    paintable.toHtml(pageContext);
  }

%></div><%
} else { // ------------------------------------------------------------ PAINTABLES ---------------------------------------------


  double scaleFactorW;
  double scaleFactorH;
  Folio folio = pasu.folio;

  scaleFactorW = folio.pixelWidth / folio.width;
  scaleFactorH = folio.pixelHeight / folio.height;

  int bt = (int)Math.round  ((pasu.top - folio.top) * scaleFactorH);
  int bl = (int)Math.round  ((pasu.left - folio.left) * scaleFactorW);


  // trimmed variables
  int top=bt;
  int left=bl;

  folio = pasu.folio;


  // when folio trims
  if (folio.trimModality){
    top = bt<0?0:bt;
    left=bl<0?0:bl;
    top=bt>folio.pixelHeight?folio.pixelHeight:bt;
    left=bl>folio.pixelWidth?folio.pixelWidth:bl;
  }



  /* -------------------------------------------------------------------------------------------------------------------

                                DRAW_RECTANGE & DRAW_LABEL & DRAW_PERCENT

 --------------------------------------------------------------------------------------------------------------------- */
  if (Paintable.DRAW_RECTANGLE.equals(request.getAttribute(Paintable.ACTION)) ||
          Paintable.DRAW_LABEL.equals(request.getAttribute(Paintable.ACTION)) ||
          Paintable.DRAW_PERCENT.equals(request.getAttribute(Paintable.ACTION))
          ) {
    Rectangle rectangle = (Rectangle) JspIncluderSupport.getCurrentInstance(request);
    int bw = (int) Math.round (rectangle.width * scaleFactorW);
    int bh = (int) Math.round (rectangle.height * scaleFactorH);

    int w=bw;
    int h=bh;

    // when folio trims
    if (folio.trimModality){
      w= bw+bl>folio.pixelWidth ? folio.pixelWidth-bl:bw;
      h=bh+bt>folio.pixelHeight?folio.pixelHeight-bt:bh;
      if((bl<folio.pixelLeft && (bl+bw)>folio.pixelLeft)){
        w=w -(folio.pixelLeft-bl);
        left=w>0?folio.pixelLeft:bl;
      }
    }

    if (rectangle.lineSize > 0 ){
      w = w - rectangle.lineSize * 2;
      h = h - rectangle.lineSize * 2;
    }
    // rectangle
    String positionStyle = "padding:2px 4px;border-radius:3px;position:absolute; left:" + left + "px; top:" + top + "px; width:" + w + "px; height:" + h + "px; ";
    String backGroundStyle = rectangle.backgroundColor == null?"":"background-color:" + rectangle.backgroundColor + "; ";
    String borderStyle="";
    if (rectangle.color != null && rectangle.lineSize != 0 && rectangle.lineStyle != null)
      borderStyle = "border:" + rectangle.lineSize + "px " + rectangle.lineStyle + " " + rectangle.color + ";";

    String colorStyle="";
    if (rectangle.color != null)
      colorStyle = "color:" + rectangle.color + ";";

    String userStyle= rectangle.style == null?"": rectangle.style;


    if (rectangle instanceof Label) { // ----------------------------------------------------- LABEL -------------------------
      Label label = (Label) rectangle;
      String content = label.label;
      String align="";
      if (label.align != null){
        align=" text-align:"+label.align+"; ";
      }
      String vAlign="";
      if (label.align != null){
        vAlign=" vertical-align :"+label.vAlign+"; ";
      }

      if (label.resizable && (label.label.length() * 10) > w)
        content = "";

      %><div <%=id%> style="<%=positionStyle%><%=backGroundStyle%><%=borderStyle%><%=colorStyle%><%=cursorStyle%><%=userStyle%><%=align%><%=vAlign%>" <%=script%> <%=additionalOnClickScript%> <%=(pasu.nowrap) ? "nowrap" : ""%> <%=toolTip%> <%=JSP.ex(pasu.htmlClass)?"class=\""+pasu.htmlClass+"\"":""%>><%=content%></div><%

    } else if (rectangle instanceof Percentage) { // -------------------------------------- PERCENTAGE -------------------------
      Percentage perc = (Percentage) rectangle;
      String fontStyle="font-size:" + (perc.fontHeight != null ? perc.fontHeight : perc.height - 2 + "px") + "; text-align:center; vertical-align:middle; ";

      int newW= (int)(w*Math.min(perc.percent,100))/100;
      String percPositionStyle = "position:absolute; left:" + left + "px; top:" + top + "px; width:" + newW + "px; height:" + h + "px; ";
      String percBackGroundStyle = perc.percentileColor == null?"":"background-color:" + perc.percentileColor + "; ";

      // background
      %><div style="<%=positionStyle%><%=backGroundStyle%><%=borderStyle%><%=colorStyle%><%=userStyle%>" <%=script%> <%=toolTip%> <%=JSP.ex(pasu.htmlClass)?"class=\""+pasu.htmlClass+"\"":""%>></div><%
      // rect
      %><div style="<%=percPositionStyle%><%=percBackGroundStyle%><%=borderStyle%><%=colorStyle%><%=fontStyle%><%=cursorStyle%><%=userStyle%>" <%=script%> <%=additionalOnClickScript%> <%=JSP.ex(pasu.htmlClass)?"class=\""+pasu.htmlClass+"\"":""%>> </div> <%
      // label
      //take care: userStyle is not printed here as it contains filter opacity which sputtans fonts anti aliasing (Pietro) 
      %><div <%=id%> style="<%=positionStyle%><%=colorStyle%><%=fontStyle%><%=cursorStyle%>" <%=script%> <%=additionalOnClickScript%> <%=(pasu.nowrap) ? "nowrap" : ""%> <%=toolTip%>  <%=JSP.ex(pasu.htmlClass)?"class=\""+pasu.htmlClass+"\"":""%>><%=perc.percent%>%</div> <%

    } else {// ---------------------------------------------------------------------------- RECTANGLE -------------------------
      positionStyle = positionStyle + "overflow:hidden; ";
      %><div <%=id%> style="<%=positionStyle%><%=backGroundStyle%><%=borderStyle%><%=colorStyle%><%=cursorStyle%><%=userStyle%>" <%=script%> <%=additionalOnClickScript%> <%=(pasu.nowrap) ? "nowrap" : ""%> <%=toolTip%> <%=JSP.ex(pasu.htmlClass)?"class=\""+pasu.htmlClass+"\"":""%>><%
      if (rectangle.jspIncluder != null) {
        rectangle.jspIncluder.toHtml(pageContext);
      }  %></div><%

    }



  /* -------------------------------------------------------------------------------------------------------------------

                                DRAW_DOT

 --------------------------------------------------------------------------------------------------------------------- */
} else if (Paintable.DRAW_DOT.equals(request.getAttribute(Paintable.ACTION))) {
  Dot dot = (Dot) JspIncluderSupport.getCurrentInstance(request);

  String finalStyle = "position:absolute; overflow:hidden; left:" + left + "px; top:" + top + "px; width:1px; height:1px; ";
  if (dot.style != null)
    finalStyle += dot.style;
  else
    finalStyle += "border-top:1px " + dot.color + " solid;";

%><div <%=id%> style="<%=finalStyle%><%=cursorStyle%>" <%=script%> <%=additionalOnClickScript%>  <%=toolTip%> <%=JSP.ex(pasu.htmlClass)?"class=\""+pasu.htmlClass+"\"":""%>>&nbsp;</div>  <%


  /* -------------------------------------------------------------------------------------------------------------------

                                DRAW_IMAGE

 --------------------------------------------------------------------------------------------------------------------- */
} else if (Paintable.DRAW_IMAGE.equals(request.getAttribute(Paintable.ACTION))) {
  Image img = (Image) JspIncluderSupport.getCurrentInstance(request);

  String finalStyle = "position:absolute; overflow:hidden; left:" + left + "px; top:" + top + "px;";
  if (img.style != null)
    finalStyle += img.style;
  else
    finalStyle += "border:none;";

%><img <%=id%> style="<%=finalStyle%><%=cursorStyle%>" <%=script%> <%=additionalOnClickScript%> <%=toolTip%> src="<%=img.imageUrl%>" <%=JSP.ex(pasu.htmlClass)?"class=\""+pasu.htmlClass+"\"":""%>><%

  /* -------------------------------------------------------------------------------------------------------------------

                                DRAW_VLINE

 --------------------------------------------------------------------------------------------------------------------- */
} else if (Paintable.DRAW_VLINE.equals(request.getAttribute(Paintable.ACTION))) {
  VLine vline = (VLine) JspIncluderSupport.getCurrentInstance(request);


  int bh = (int) Math.round (vline.height * scaleFactorH);
  int h=bh;

  // when folio trims
  if (folio.trimModality){
    h=bh+bt>folio.pixelHeight?folio.pixelHeight-bt:bh;
  }

    String finalStyle = "position:absolute; overflow:hidden; left:" + (left - vline.lineSize/2) + "px; top:" + top + "px; width:"+vline.lineSize+"px; height:" + h + "px; ";
  if (vline.style != null)
    finalStyle += vline.style;
  else
    finalStyle += "border-left:" + vline.lineSize + "px " + vline.lineStyle + " " + vline.color + ";";
      //finalStyle += "background-color:" + vline.color + ";";

%><div <%=id%> style="<%=finalStyle%><%=cursorStyle%>" <%=script%> <%=additionalOnClickScript%> <%=toolTip%> <%=JSP.ex(pasu.htmlClass)?"class=\""+pasu.htmlClass+"\"":""%>></div><%


  /* -------------------------------------------------------------------------------------------------------------------

                                DRAW_HLINE

 --------------------------------------------------------------------------------------------------------------------- */
} else if (Paintable.DRAW_HLINE.equals(request.getAttribute(Paintable.ACTION))) {
  HLine hline = (HLine) JspIncluderSupport.getCurrentInstance(request);

    int bw = (int) Math.round(hline.width * scaleFactorW);
  int w=bw;

  // when folio trims
  if (folio.trimModality){
    w= bw+bl>folio.pixelWidth ? folio.pixelWidth-bl:bw;
  }
        
    String  finalStyle = "position:absolute; overflow:hidden; left:" + left + "px; top:" + (top-hline.lineSize/2 ) + "px; width:" + w + "px; height:"+hline.lineSize+"px; ";
    if (hline.style != null)
      finalStyle += hline.style;
    else
      finalStyle += "border-top:" + hline.lineSize + "px " + hline.lineStyle + " " + hline.color + ";";
      //finalStyle += "background-color:" + hline.color + ";";


    %> <div <%=id%> style="<%=finalStyle%><%=cursorStyle%>" <%=script%> <%=additionalOnClickScript%>  <%=toolTip%> <%=JSP.ex(pasu.htmlClass)?"class=\""+pasu.htmlClass+"\"":""%>></div> <%


    /* -------------------------------------------------------------------------------------------------------------------

                                  DRAW_HLINE

   --------------------------------------------------------------------------------------------------------------------- */
  } else if (Paintable.DRAW_HLINE.equals(request.getAttribute(Paintable.ACTION))) {
    HLine hline = (HLine) JspIncluderSupport.getCurrentInstance(request);

    int bw = (int) Math.round(hline.width * scaleFactorW);
    int w=bw;

    // when folio trims
    if (folio.trimModality){
      w= bw+bl>folio.pixelWidth ? folio.pixelWidth-bl:bw;
    }

    String  finalStyle = "position:absolute; overflow:hidden; left:" + left + "px; top:" + (top-hline.lineSize/2 ) + "px; width:" + w + "px; height:"+hline.lineSize+"px; ";
  if (hline.style != null)
    finalStyle += hline.style;
  else
    finalStyle += "border-top:" + hline.lineSize + "px " + hline.lineStyle + " " + hline.color + ";";
      //finalStyle += "background-color:" + hline.color + ";";


    %> <div <%=id%> style="<%=finalStyle%><%=cursorStyle%>" <%=script%> <%=additionalOnClickScript%>  <%=toolTip%> <%=JSP.ex(pasu.htmlClass)?"class=\""+pasu.htmlClass+"\"":""%>></div> <%

  /* -------------------------------------------------------------------------------------------------------------------

                                DRAW_LINE

 --------------------------------------------------------------------------------------------------------------------- */
} else if (Paintable.DRAW_LINE.equals(request.getAttribute(Paintable.ACTION))) {
  Line line = (Line) JspIncluderSupport.getCurrentInstance(request);

  int bw = (int) Math.round(line.width * scaleFactorW);
  int bh = (int) Math.round(line.height * scaleFactorH);

  int w = bw;
  int h = bh;

  // when folio trims
  if (folio.trimModality) {
    w = bw + bl > folio.pixelWidth ? folio.pixelWidth - bl : bw;
    h = bh + bt > folio.pixelHeight ? folio.pixelHeight - bt : bh;
    if ((bl < folio.pixelLeft && (bl + bw) > folio.pixelLeft)) {
      w = w - (folio.pixelLeft - bl);
      left = w > 0 ? folio.pixelLeft : bl;
    }
  }

  if (line.lineSize > 1) {
    drawLine2d(left, top, left + w, top + h, line.lineSize, line.color, pageContext);
  } else if (line.lineSize == 1) {
    drawLine(left, top, left + w, top + h, line.color, pageContext);
  } else {
    drawLineDot(left, top, left + w, top + h, line.color, pageContext);
  }

  /* -------------------------------------------------------------------------------------------------------------------

                                DRAW_ROUNDEDBOX

 --------------------------------------------------------------------------------------------------------------------- */
} else if (Paintable.DRAW_ROUNDED.equals(request.getAttribute(Paintable.ACTION))) {

  RoundedRectangle rounded = (RoundedRectangle) JspIncluderSupport.getCurrentInstance(request);
  int bw = (int) Math.round(rounded.width * scaleFactorW);
  int bh = (int) Math.round(rounded.height * scaleFactorH);

  int width = bw;
  int height = bh;

  // when folio trims
  if (folio.trimModality) {
    width = bw + bl > folio.pixelWidth ? folio.pixelWidth - bl : bw;
    height = bh + bt > folio.pixelHeight ? folio.pixelHeight - bt : bh;
    if ((bl < folio.pixelLeft && (bl + bw) > folio.pixelLeft)) {
      width = width - (folio.pixelLeft - bl);
      left = width > 0 ? folio.pixelLeft : bl;
    }
  }


  String positionStyle = "position:absolute; left:" + left + "px; top:" + top + "px; width:" + width + "px; height:"+height+"px; overflow:hidden; ";

  String colorStyle = (rounded.style==null?"":rounded.style);
  if (rounded.color != null)
    colorStyle += "color:" + rounded.color + ";";


    int border=rounded.roundCurve;
%><div <%=id%> <%=toolTip%> style="<%=positionStyle%><%=colorStyle%><%=cursorStyle%> padding:<%=border%>"  <%=script%> <%=additionalOnClickScript%>  <%=JSP.ex(pasu.htmlClass)?"class=\""+pasu.htmlClass+"\"":""%>>

 <div style="background:transparent url(<%=rounded.templateImage%>) no-repeat top left;     position:absolute; top:0px; left:0px; height:<%=height-border%>px;  width:<%=border%>px; font-size:1px; " ></div>
 <div style="background:transparent url(<%=rounded.templateImage%>) no-repeat bottom left;  position:absolute; top:<%=height-border%>px; left:0px; height:<%=border%>px; width:<%=width-border%>px; font-size:1px;"></div>
 <div style="background:transparent url(<%=rounded.templateImage%>) no-repeat top right;    position:absolute; top:0px; left:<%=border%>px; height:<%=height-border%>px; width:<%=width-border%>px; font-size:1px;" ></div>
 <div style="background:transparent url(<%=rounded.templateImage%>) no-repeat bottom right; position:absolute; top:<%=border%>px; left:<%=width-border%>px; height:<%=height-border%>px; width:<%=border%>px;font-size:1px;"></div>
 <div style="position:absolute; top:<%=border%>px; left:<%=border%>px; height:<%=height-border*2%>px;  width:<%=width-border*2%>px; overflow:hidden; "><%
          if (rounded.jspIncluder != null) {
            rounded.jspIncluder.toHtml(pageContext);
          }
        %></div></div><%


  }
 }
%>

