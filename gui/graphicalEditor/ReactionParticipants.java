package fr.inria.contraintes.biocham.graphicalEditor;

import java.io.Serializable;
import java.util.Vector;

public class ReactionParticipants implements Serializable{
	
	protected Vector molecule;//, products, modulators1;
	
	public ReactionParticipants(){
		molecule = new Vector();
		//products = new Vector();
		//modulators1 = new Vector();
	}

//	 REACTANTS
	 public MoleculeEntity[] getMolecules()
	    {
		 
		 MoleculeEntity[] retThingies = new MoleculeEntity[molecule.size()];
	        if (molecule.size() > 0) molecule.copyInto(retThingies);

	        return retThingies;
	    }

	    public void setMolecules(MoleculeEntity[] newThingies)
	    {
	    	molecule = new Vector(newThingies.length);
	        for (int i=0; i < newThingies.length; i++)
	        {
	        	molecule.addElement(newThingies[i]);
	        }
	    }

	    public MoleculeEntity getMolecules(int i)
	    {
	        return (MoleculeEntity) molecule.elementAt(i);
	    }

	    public void setMolecules(int i, MoleculeEntity thingy)
	    {
	    	molecule.setElementAt(thingy, i);
	    }
	    
	    
	    
	    
	    
	 /*   //PRODUCTS
	    public MoleculeEntity[] getProducts()
	    {
		 
	    	MoleculeEntity[] retThingies = new MoleculeEntity[products.size()];
	        if (products.size() > 0) products.copyInto(retThingies);

	        return retThingies;
	    }

	    public void setProducts(MoleculeEntity[] newThingies)
	    {
	    	products = new Vector(newThingies.length);
	        for (int i=0; i < newThingies.length; i++)
	        {
	        	products.addElement(newThingies[i]);
	        }
	    }

	    public MoleculeEntity getProducts(int i)
	    {
	        return (MoleculeEntity) products.elementAt(i);
	    }

	    public void setProducts(int i, MoleculeEntity thingy)
	    {
	    	products.setElementAt(thingy, i);
	    }
	    
	    
	    
	    
	    
	    //MODULATORS
	    public MoleculeEntity[] getModulators()
	    {
		 
	    	MoleculeEntity[] retThingies = new MoleculeEntity[modulators1.size()];
	        if (modulators1.size() > 0) modulators1.copyInto(retThingies);

	        return retThingies;
	    }

	    public void setModulators(MoleculeEntity[] newThingies)
	    {
	    	modulators1 = new Vector(newThingies.length);
	        for (int i=0; i < newThingies.length; i++)
	        {
	        	modulators1.addElement(newThingies[i]);
	        }
	    }

	    public MoleculeEntity getModulators(int i)
	    {
	        return (MoleculeEntity) modulators1.elementAt(i);
	    }

	    public void setModulators(int i, MoleculeEntity thingy)
	    {
	    	modulators1.setElementAt(thingy, i);
	    }
	*/
	
	
	
}
