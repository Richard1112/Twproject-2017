package pt.tumba.ngram;

import java.io.*;
import java.util.*;

/**
 * Reads data from two N-gram profile files and build a new one with
 * basis on the most informative N-grams in comparing both profiles.
 * Profiles are responsible for storing N-gram occurence frequency
 * information for a given textual string.
 *
 * @author Bruno Martins
 */
public class MostInformativeProfile implements Profile {

    /**
     *  The name for the Profile (usually equal to the filename from where it was read).
     */
    protected String name;

    /**
     * The name for the first Profile (usually equal to the filename from where it was read).
     */
    protected String name1;
    
    /**
     * The name for the first Profile (usually equal to the filename from where it was read).
     */
    protected String name2;
    
    /**
     * The first profile.
     */
    protected DataProfile profile1;
    
    /**
     * The second profile.
     */
    protected DataProfile profile2;

    /**
     * The list of N-grams in this Profile.
     */
    protected List rankedNGrams;
    
    /**
     * Constructor for <code>MostInformativeProfile</code>.
     *
     *@param  name    The name of the profile.
     */
    public MostInformativeProfile(String name) {
    	this.name = name;
    }

    /**
     *  Constructor for <code>MostInformativeProfile</code>.
     *
     *@param  name    The name of the profile.
     *@param  stream1  An <code>InputStream</code> from where to read the first Profile.
     *@param  stream2  An <code>InputStream</code> from where to read the second Profile.
     */
    public MostInformativeProfile(String name1, String name2, InputStream stream1, InputStream stream2) throws IOException {
        this(name1+":"+name2);
        this.name1 = name1;
        this.name2 = name2;
        rankedNGrams = computeMostInformative(name1,name2,stream1,stream2,2);
    }

    /**
     *  Constructor for <code>MostInformativeProfile</code>.
     *
     *@param  stream1  An <code>InputStream</code> from where to read the first Profile.
     *@param  stream2  An <code>InputStream</code> from where to read the second Profile.
     *@param  z  The value for the significance test.
     */
    public MostInformativeProfile(String name1, String name2, InputStream stream1, InputStream stream2, double z) throws IOException {
        this(name1+":"+name2);
        this.name1 = name1;
        this.name2 = name2;
        rankedNGrams = computeMostInformative(name1,name2,stream1,stream2,z);
    }

    /**
     *  Gets the name of the Profile.
     *
     *@return    The name of the Profile.
     */
    public String getName() {
        return name;
    }

    /**
     *  Gets the name of the first Profile.
     *
     *@return    The name of the Profile.
     */
    public String getNameFirst() {
        return name1;
    }
    
    /**
     *  Gets the name of the second Profile.
     *
     *@return    The name of the Profile.
     */
    public String getNameSecond() {
        return name2;
    }

    /**
     *  Gets the first Profile.
     *
     *@return    The Profile.
     */
    public Profile getFirst() {
        return profile1;
    }
    
    /**
     *  Gets the second Profile.
     *
     *@return    The Profile.
     */
    public Profile getSecond() {
        return profile2;
    }

    
    /**
     *  Gets the ranking position of a given N-gram.
     *
     *@param  gram  An N-Gram
     *@return  The associated ranking position.
     */
    public double getRank(NGram gram) {
        Iterator iter = ngrams();
        int i = 0, lastPos = 0, lastCount = 0;
        while (iter.hasNext()) {
            i++;
            NGram g = (NGram)(iter.next());
            double count = g.getCount();
            if(count!=lastCount) lastPos = i;
            if (((NGram) iter.next()).bytes.equals(gram.bytes)) {
				while (iter.hasNext()) {
					g = (NGram)(iter.next());
					count = g.getCount();
					if(count!=lastCount) return (lastPos + i) * 0.5;
					i++;
				}
				return (lastPos + i) * 0.5;
            } 
        }
        return 0;
    }

	/**
	 *  Gets the weighting score of a given N-gram.
	 *
	 *@param  gram  An N-Gram
	 *@return  The associated occurence frequency.
	 */
	public double getWeight(NGram gram) {
		Iterator iter = ngrams();
		int i = 0;
		while (iter.hasNext()) {
			i++;
			NGram gram2 = (NGram) iter.next();
			if (gram2.bytes.equals(gram.bytes)) {
				return gram2.getCount();
			} 
		}
		return 0;
	}

	/**
	 *  Gets the weighting score of a given N-gram.
	 *
	 *@param  gram  An N-Gram
	 *@return  The associated occurence frequency.
	 */
	public double getInformationGain(NGram gram, int n) {
		Iterator iter = ngrams();
		int i = 0;
		while (iter.hasNext()) {
			i++;
			GramFromTwoProfiles gram2 = (GramFromTwoProfiles) iter.next();
			if (gram2.gram.equals(new String(gram.bytes))) {
				if(n==1) return gram2.relativeFrequencyProfile1 - gram2.relativeFrequencyProfile2;
				else return gram2.relativeFrequencyProfile2 - gram2.relativeFrequencyProfile1;
			} 
		}
		return 0;
	}

	/**
     *  Gets the number of N-grams in the Profile.
     *
     *@return The number of N-grams in the Profile.
     */
    public int getSize() {
        return rankedNGrams.size();
    }


    /**
     *  Returns an <code>Iterator</code> over the N-grams in this profile.
     *
     *@return An <code>Iterator</code> over the N-grams in this profile.
     */
    public Iterator ngrams() {
        return rankedNGrams.iterator();
    }
	
    /**
     * Converts this profile to a String Object.
     *
     *@return    A String with the name of this <code>Profile</code>.
     */
    public String toString() {
        return getName();
    }

    /**
     * Computes the most informative N-grams from two N-gram profiles.
     * 
     * @throws IOException A problem occured while reading the profiles.
     */
    private List computeMostInformative (String name1, String name2, InputStream str1, InputStream str2, double zValue) throws IOException {
    	Map finalGrams = new HashMap();
    	
    	Map grams1 = new HashMap();
		Map grams2 = new HashMap();
		Iterator it1 = ProfileReader.read(str1).iterator();
		double totalFreq1 = 0;
		while(it1.hasNext()) {
			NGram gram = ((NGram)(it1.next()));
			grams1.put(gram,new Double(gram.getCount()));
			totalFreq1 += gram.getCount();
		}
		it1 = grams1.keySet().iterator();
		Iterator it2 = ProfileReader.read(str2).iterator();
		double totalFreq2 = 0;
		while(it2.hasNext()) {
			NGram gram = ((NGram)(it2.next()));
			grams2.put(gram,new Double(gram.getCount()));
			totalFreq2 += gram.getCount();
		}
		it2 = grams2.keySet().iterator();
		while(it1.hasNext()) {
			String gram = (String)(it1.next().toString());
			GramFromTwoProfiles g = new GramFromTwoProfiles(gram,(Double)(grams1.get(gram)),(Double)(grams2.get(gram)),totalFreq1,totalFreq2); 
			if(zValue==-1 || g.z>=zValue) finalGrams.put(g,null);
		}
		while(it2.hasNext()) {
			String gram = (String)(it2.next().toString());
			GramFromTwoProfiles g = new GramFromTwoProfiles(gram,(Double)(grams1.get(gram)),(Double)(grams2.get(gram)),totalFreq1,totalFreq2); 
			if(finalGrams.containsKey(g)) continue;
			if(zValue==-1 || g.z>=zValue) finalGrams.put(g,null);
		}
		List result = new Vector(finalGrams.keySet());
		List list1 = new Vector(grams1.keySet());
		List list2 = new Vector(grams2.keySet());
		Collections.sort(result,new GramFromTwoProfiles());
		Collections.sort(list1,new GramFromTwoProfiles());
		Collections.sort(list2,new GramFromTwoProfiles());
		profile1 = new DataProfile(name1,list1);
		profile2 = new DataProfile(name2,list2);

		//Hacked by Nuno
		PrintStream prev = System.out;
		System.setOut(new PrintStream(new FileOutputStream("n-gram.dat")));
		
		System.out.println(totalFreq1+"\t"+totalFreq2);
		for (int i =0; i<result.size(); i++ ) {
			GramFromTwoProfiles gram = (GramFromTwoProfiles)(result.get(i));
			System.out.println(gram.gram+"\t"+gram.delta+
					                     "\t"+gram.frequencyProfile1+
										 "\t"+gram.frequencyProfile2+
										 "\t"+gram.z);
		}
		System.out.close();
		System.setOut(prev);
		return result;
    }
	
	/**
	 * The main method for the MostInformativeProfile class.
	 *
	 * @param  args             The command line arguments, tokenized.
	 * @exception  IOException  A problem occurred while processing files.
	 */
    public static void main ( String args[] ) throws Exception {
		MostInformativeProfile p = new MostInformativeProfile("test");
		p.computeMostInformative("","",new FileInputStream(args[0]),new FileInputStream(args[1]),-1);
	}

	/**
	 * Inner class for storing and comparing N-grams from two profiles.
	 */
    class GramFromTwoProfiles extends NGram implements Comparator {
	
		/** The frequency for the N-Gram in the first profile. */
    	double frequencyProfile1;
		
    	/** The frequency for the N-Gram in the second profile. */
    	double frequencyProfile2;
		
    	/** The relative frequency for the N-Gram in the first profile. */
    	double relativeFrequencyProfile1;
		
    	/** The relative frequency for the N-Gram in the second profile. */
    	double relativeFrequencyProfile2;
		
    	/** The absolute value of the difference between the relative frequencies of the N-Gram in both profiles. */
    	double delta;
    	
    	/** The test statistic */
    	double z;
		
    	/** The N-Gram string. */
    	String gram;

		/**
		 * Constructor for an uninitialized <code>GramFromTwoProfiles</code> class. 
		 */
    	GramFromTwoProfiles () {
		}

    	/**
    	 *
		 * Constructor for <code>GramFromTwoProfiles</code>.
		 * 
    	 * @param gram  The N-Gram string.
    	 * @param frequencyProfile1 The frequency for the N-Gram in the first profile.
    	 * @param frequencyProfile2 The frequency for the N-Gram in the second profile.
    	 * @param totalFreq1 The total number of N-Grams in the first profile.
    	 * @param totalFreq2 The total number of N-Grams in the second profile.
    	 */
    	GramFromTwoProfiles (String gram, Double frequencyProfile1, Double frequencyProfile2, double totalFreq1, double totalFreq2) {
			super(gram);
			this.gram = gram;
			this.frequencyProfile1 = (frequencyProfile1==null) ? 0: frequencyProfile1.doubleValue();
			this.frequencyProfile2 = (frequencyProfile2==null) ? 0: frequencyProfile2.doubleValue();
			this.relativeFrequencyProfile1 = this.frequencyProfile1 / totalFreq1; 
			this.relativeFrequencyProfile2 = this.frequencyProfile2 / totalFreq2;
			this.delta = this.relativeFrequencyProfile1 - this.relativeFrequencyProfile2;
			this.z = this.delta / Math.sqrt((relativeFrequencyProfile1 * (1-relativeFrequencyProfile1) / totalFreq1) +	(relativeFrequencyProfile2 * (1-relativeFrequencyProfile2) / totalFreq2));	
			super.count = Math.abs(z);
    	}
		
    	/**
    	 * Compare two <code>GramFromTwoProfiles</code> objects with basis on the delta value.
    	 * @param o1 An instance of <code>GramFromTwoProfiles</code>.
    	 * @param o2 Another instance of <code>GramFromTwoProfiles</code>.
    	 * @return The result from the comparison on the delta value.
    	 */
    	public int compare(Object o1, Object o2) {
		 	if(o1 instanceof GramFromTwoProfiles && o2 instanceof GramFromTwoProfiles) {
		 		GramFromTwoProfiles g1 = (GramFromTwoProfiles)o1;
		 		GramFromTwoProfiles g2 = (GramFromTwoProfiles)o2;
		 		if(g1.z==g2.z) return 0;
		 		else if(g1.z<g2.z) return 1;
		 		else return -1;
		 	} else {
		 		NGram g1 = (NGram)o1;
		 		NGram g2 = (NGram)o2;
		 		if(g1.count==g2.count) return 0;
		 		else if(g1.count<g2.count) return 1;
		 		else return -1;
		 	}
		}
		 
        /**
         * Returns true if another object implements the same <code>Comparator</code>.
         */ 
    	public boolean	equals(Object obj) {
         	return (obj instanceof GramFromTwoProfiles) || (obj instanceof NGram);
         }
    	
    	/**
    	 * Returns an hash value for this NGram with basis on the NGram String.
    	 */
    	public int hashCode() {
			return (this.gram + this.frequencyProfile1 + this.frequencyProfile2	+ this.relativeFrequencyProfile1 + this.relativeFrequencyProfile2+this.delta+this.z).hashCode();
    	}
		
	}
    
}