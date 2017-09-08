package com.twproject.messaging.stickyNote;

import com.twproject.messaging.board.Board;
import com.twproject.resource.Person;
import org.jblooming.ontology.LoggableIdentifiableSupport;

import java.io.Serializable;
import java.util.Date;

import net.sf.json.JSONObject;
import org.jblooming.waf.html.layout.HtmlColors;

/**
 * @author pietro polsinelli info@twproject.com
 */
public class StickyNote extends LoggableIdentifiableSupport  {

  private String type;
  private String title;
  private Person author;
  private Person receiver;
  private String message;
  private Date readOn;
  private int x;
  private int y;
  private int w;
  private int h;
  private boolean iconized;

  private String color="#ffff80";

  private Board board;

  public StickyBricks bricks = new StickyBricks(this);


  public StickyNote() {
  }

  public Person getAuthor() {
    return author;
  }

  public void setAuthor(Person author) {
    this.author = author;
  }

  public Person getReceiver() {
    return receiver;
  }

  public void setReceiver(Person receiver) {
    this.receiver = receiver;
  }

  public String getMessage() {
    return message;
  }

  public void setMessage(String message) {
    this.message = message;
  }

  public Date getReadOn() {
    return readOn;
  }

  public void setReadOn(Date readOn) {
    this.readOn = readOn;
  }

  public int getX() {
    return x;
  }

  public void setX(int x) {
    this.x = x;
  }

  public int getY() {
    return y;
  }

  public void setY(int y) {
    this.y = y;
  }

  public int getW() {
    return w;
  }

  public void setW(int w) {
    this.w = w;
  }

  public int getH() {
    return h;
  }

  public void setH(int h) {
    this.h = h;
  }

  public boolean isIconized() {
    return iconized;
  }

  public void setIconized(boolean iconized) {
    this.iconized = iconized;
  }

  public String getType() {
    return type;
  }

  public void setType(String type) {
    this.type = type;
  }

  public String getTitle() {
    return title;
  }

  public void setTitle(String title) {
    this.title = title;
  }


  public String getName() {
    return getMessage();
  }

  public String getColor() {
    return color;
  }

  public void setColor(String color) {
    this.color = color;
  }


  public Board getBoard() {
    return board;
  }

  public void setBoard(Board board) {
    this.board = board;
  }

  public JSONObject jsonify() {
    JSONObject ret = super.jsonify();
    ret.element("id", getId());
    ret.element("type", getType());
    ret.element("title", getTitle());
    ret.element("message", getMessage());
    ret.element("color", getColor());
    ret.element("contrastColor",  HtmlColors.contrastColor(getColor()));
    ret.element("lastModifiedMillis", getLastModified().getTime());
    ret.element("creationMillis", getCreationDate().getTime());
    if (getReadOn()!=null)
      ret.element("readOnMillis", getReadOn().getTime());
    Person auth = getAuthor();
    if (auth !=null){
      ret.element("authorId", auth.getId());
      ret.element("authorName", auth.getDisplayName());
    }
    Person rec = getReceiver();
    if (rec!=null){
      ret.element("receiverId", rec.getId());
      ret.element("receiverName", rec.getDisplayName());
    }
    return ret;
  }

}
