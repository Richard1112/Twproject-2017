package org.jblooming.waf.html.layout;

import org.jblooming.utilities.StringUtilities;


public class HtmlColors {

  /**
   * @param col html color string example: #0000ff
   * @param iR  red channel increment ex: 16
   * @return modified color ex: #1000ff
   */
  public static String changeRed(String col, int iR) {
    return modifyLight(col, iR, 0, 0);
  }

  /**
   * @param col html color string example: #0000ff
   * @param iG  green channel increment ex: 16
   * @return modified color ex: #0010ff
   */
  public static String changeGreen(String col, int iG) {
    return modifyLight(col, 0, iG, 0);
  }

  /**
   * @param col html color string example: #2CC02C
   * @param iB  green channel increment ex: 16
   * @return modified color ex: #00ff10
   */
  public static String changeBlue(String col, int iB) {
    return modifyLight(col, 0, 0, iB);
  }

  /**
   * @param col html color string example: #101010
   * @param i   all channel increment ex: 16
   * @return modified color ex: #202020
   */
  public static String modifyLight(String col, int i) {
    String cc = modifyLight(stringToColor(col), i, i, i);
    return cc;
  }

  /**
   * @param col html color string example: #101010
   * @param iR  red channel increment ex: 16
   * @param iG  green channel increment ex: 17
   * @param iB  blue channel increment ex: 18
   * @return modified color ex: #202122
   */
  public static String modifyLight(String col, int iR, int iG, int iB) {
    String cc = modifyLight(stringToColor(col), iR, iG, iB);
    return cc;
  }

  /**
   * @param col html color string example: #101010
   * @return complementar color  #efefef
   */
  public static String complementarColor(String col) {
    String cc = complementarColor(stringToColor(col));
    return cc;
  }

  /**
   * @param col html color string example: #f3f3f3
   * @return the most contrast (blak or white) on color
   */
  public static String contrastColor(String col) {
    String cc = contrastColor(stringToColor(col));
    return cc;
  }

  /**
   * @param col html color string example: #f3f3f3
   * @return the most contrast (blak or white) on color
   */
  public static boolean contrastImg(String col) {
    boolean img = contrastImg(stringToColor(col));
    return img;
  }


  /**
   * @param col html color string example: #102030
   * @return return color shift r->g->b->r ex: #203010 ;
   *         b = o_r;
   */
  public static String shiftColor(String col) {
    String cc = shiftColor(stringToColor(col));
    return cc;
  }


  /**
   * @param col html color string example: #102030
   * @param sat saturation % (-100:100)
   * @return return grayer color if sat>0 : return colorer if sat<0
   */
  public static String modifySaturation(String col, int sat) {
    Color color = stringToColor(col);
    int r = color.r;
    int g = color.g;
    int b = color.b;
    int mean = (r + g + b) / 3;

    color.r = color.r + ((mean - color.r) * sat) / 100;
    color.g = color.g + ((mean - color.g) * sat) / 100;
    color.b = color.b + ((mean - color.b) * sat) / 100;

    return colorToStringHex(color);
  }


  /**
   * @param colstr html color string example: #f3f3f3 or rgb(1,2,3)
   * @return return the Color instance of html color
   */
  public static Color stringToColor(String colstr) {
    Color col = new Color(0, 0, 0);
    if (colstr != null && colstr.length() >= 7) {
      if (colstr.startsWith("#")) {
        col = new Color(Integer.valueOf(colstr.substring(1, 3), 16).intValue(),
                Integer.valueOf(colstr.substring(3, 5), 16).intValue(),
                Integer.valueOf(colstr.substring(5), 16).intValue());
      } else if (colstr.toLowerCase().startsWith("rgb")) {
        String[] cols = StringUtilities.splitToArray(colstr.substring(colstr.indexOf("(")+1, colstr.indexOf(")")), ",");
        col = new Color(Integer.valueOf(cols[0].trim()).intValue(), Integer.valueOf(cols[1].trim()).intValue(), Integer.valueOf(cols[2].trim()).intValue());
      }
    }

    return col;

  }


  private static String modifyLight(Color col, int iR, int iG, int iB) {
    int r, g, b;
    Color color;
    r = col.getRed();
    g = col.getGreen();
    b = col.getBlue();

    r = addToHex(r, iR);
    g = addToHex(g, iG);
    b = addToHex(b, iB);
    color = new Color(r, g, b);
    return colorToStringHex(color);
  }


  private static String complementarColor(Color col) {
    int r, g, b;
    String sc;
    Color color;
    r = col.getRed();
    g = col.getGreen();
    b = col.getBlue();
    r = 255 - r;
    g = 255 - g;
    b = 255 - b;

    r = addToHex(r, 0);
    g = addToHex(g, 0);
    b = addToHex(b, 0);
    color = new Color(r, g, b);
    sc = colorToStringHex(color);
    return sc;
  }

  private static String contrastColor(Color col) {
    int r, g, b, tot;
    Color color;
    r = col.getRed();
    g = col.getGreen();
    b = col.getBlue();
    tot = (r + g + b) / 3;
    String ret="#FFFFFF";
    if (tot > 170)
      ret="#000000";
    return ret;
  }

  private static boolean contrastImg(Color col) {
    int r, g, b, tot;
    boolean contrast;
    r = col.getRed();
    g = col.getGreen();
    b = col.getBlue();
    tot = (r + g + b) / 3;
    if (tot > 128) {
      contrast = true;
    } else {
      contrast = false;
    }
    return contrast;
  }


  private static String shiftColor(Color col) {
    int r, g, b, o_r;
    Color color;
    r = col.getRed();
    g = col.getGreen();
    b = col.getBlue();
    o_r = r;
    r = g;
    g = b;
    b = o_r;
    color = new Color(r, g, b);
    return colorToStringHex(color);
  }

  public static String colorToStringHex(Color col) {
    String str, rh, gh, bh;
    int r, g, b;

    r = col.getRed();
    g = col.getGreen();
    b = col.getBlue();
    rh = intToHex(r);
    gh = intToHex(g);
    bh = intToHex(b);
    str = '#' + rh + gh + bh;
    return str.toUpperCase();
  }

  private static int addToHex(int v, int inc) {

    v = v + inc;
    if (v > 255) v = 255;
    if (v < 0) v = 0;
    return v;

  }

  private static String intToHex(int i) {
    return Integer.toString((i & 0xff) + 0x100, 16).substring(1);
  }

  public static String getDepthColor(int depth) {
    int sat = 5 * (depth);
    //todo fare qualcos'altro
    return modifyLight(modifySaturation("#F8F8F8", (sat > 100 ? 100 : (sat < -100 ? -100 : sat))), 5 * (depth));
  }

  // r,g,b values are from 0 to 1
  // h = [0,360], s = [0,1], v = [0,1]
  //  if s == 0, then h = -1 (undefined)
  public static String HSVtoRGB(float h, float s, float v) {
    int i;
    double f, p, q, t;
    double r, g, b;

    if (s == 0) {
      // achromatic (grey)
      return "555555";
    }

    h = h / 60;   // sector 0 to 5
    i = (int) Math.floor(h);
    f = h - i;   // factorial part of h
    p = v * (1 - s);
    q = v * (1 - s * f);
    t = v * (1 - s * (1 - f));

    switch (i) {
      case 0:
        r = v;
        g = t;
        b = p;
        break;
      case 1:
        r = q;
        g = v;
        b = p;
        break;
      case 2:
        r = p;
        g = v;
        b = t;
        break;
      case 3:
        r = p;
        g = q;
        b = v;
        break;
      case 4:
        r = t;
        g = p;
        b = v;
        break;
      default:  // case 5:
        r = v;
        g = p;
        b = q;
        break;
    }
    return colorToStringHex(new Color((int) (r * 255), (int) (g * 255), (int) (b * 255)));
  }

  public static String distributeColor(int elementNumber, int collectionSize) {
    return HSVtoRGB((360.0F / collectionSize) * elementNumber, 0.8F, 1F);
  }


  public static class Color {
    int r, g, b;

    public Color(int r, int g, int b) {
      this.r = r;
      this.g = g;
      this.b = b;
    }

    public int getGreen() {
      return g;
    }

    public int getRed() {
      return r;
    }

    public int getBlue() {
      return b;
    }
  }


  /**
   * @param value tra zero e uno
   * @return un colore da verde al rosso passando dal giallo. 0=verde .5=giallo 1=rosso
   */
  public static String getGreenToRed(double value){
    double step= Math.round(value*10d)/10d;
    return HtmlColors.HSVtoRGB(128f - (float)step *128f , 1f, .9f);
  }

}
