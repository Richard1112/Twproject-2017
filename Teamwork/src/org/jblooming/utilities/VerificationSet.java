package org.jblooming.utilities;

import java.util.Collection;
import java.util.HashSet;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class VerificationSet extends HashSet {

  private boolean unremovable;
  private boolean admitsDuplicates;

  private VerificationSet(Collection c, boolean unremovable, boolean admitsDuplicates) {
    super(c);
    this.unremovable = unremovable;
    this.admitsDuplicates = admitsDuplicates;
  }

  public static VerificationSet getDefiningSetInstance() {
    return new VerificationSet(new HashSet(), true, false);
  }

  public static VerificationSet getVerificationSetInstance(Collection c) {
    return new VerificationSet(c, false, true);
  }

  public boolean remove(Object o) {
    if (unremovable)
      throw new RuntimeException("Unremovable instance");
    if (!this.contains(o))
      throw new RuntimeException("Object not present: " + o.toString());
    return super.remove(o);
  }

  public void verify(VerificationSet compare) {
    if (!this.equals(compare))
      throw new RuntimeException("Sets are different; this.size=" + this.size() + " compare.size=" + compare.size());
  }

  public boolean add(Object o) {
    //if (!admitsDuplicates && this.contains(o))
    //  throw new RuntimeException("Parameter already present: " + o.toString());
    return super.add(o);
  }

}
