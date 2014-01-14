package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.event.GraphSelectionEvent;
import org.jgraph.graph.DefaultGraphCell;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.UUID;




public class BiochamEntityData extends BiochamCellData{
	
	
	String initialConcentration="0.0";
	String moleculeState="NONE";
	String compartment="";
	int multimerCardinality=1;
	ArrayList<ContainingMolecule> containingMolecules;	
	Color color=null;
	BiochamGraph graph;
	String name, representingName, moleculeType;
	UUID id;
	Position position;
	MoleculeSize size;
	int numberOfConnections=0;
	ArrayList<String> involvedInReactions;
	boolean reactant, product, modulator, responsable;
	boolean copy;
	int copyInstance=0;
	String modificationSites;
	int numberOfModifSites=0;
	boolean compact=false;
	boolean setAsCompact=false;
	boolean toDelete=false;
	String stoichoimetryForReaction;
	Rectangle2D bounds;
	boolean setByAlgo=false;
	int modulatorPosition;
	boolean moreRolePerReaction=false;
	
	public String getModificationSites() {
		return modificationSites;
	}

	public void setModificationSites(String modificationSites) {
		this.modificationSites = modificationSites;
	}

	public BiochamEntityData(BiochamGraph g){
		this(null,g);
	}
	public BiochamEntityData(BiochamGraph g,ContainingMolecule m){
		this(null,g);
		this.setName(m.getName());
		this.setColor(m.getColor());
		this.setModulator(m.isModulator());
		this.setMultimerCardinality(m.getCardinality());
		this.setInitialConcentration(m.getInitialConcentration());
		this.setId(m.getId());
		this.setMoleculeType(m.getMoleculeType());
		this.setMoleculeState(m.getState());
		this.setRepresentingName(m.getRepresentingName());	
		this.setStoichoimetryForReaction(m.getReactionStoichiometry());
	}
	
	public BiochamEntityData(Map props,BiochamGraph g){
		
		properties=new Hashtable(MIN_NUMBER_OF_PROPERTIES+2);
		graph=g;
		involvedInReactions=new ArrayList<String>();
	}

	

	public BiochamEntityData(BiochamEntityData data) {
		this(null,data.getGraph());
		initialConcentration=data.getInitialConcentration();
		moleculeState=data.getMoleculeState();
		compartment=data.getCompartment();
		multimerCardinality=data.getMultimerCardinality();
		containingMolecules=data.getContainingMolecules();	
		color=data.getColor();
		name=data.getName();
		representingName=data.getRepresentingName();
		moleculeType=data.getMoleculeType();
		id=data.getId();
		position=data.getPosition();
		size=data.getSize();
		numberOfConnections=data.getNumberOfConnections();
		involvedInReactions=data.getInvolvedInReactions();		
		modulator=data.isModulator();
		responsable=data.isResponsable();
		copy=data.isCopy();
		copyInstance=data.getCopyInstance();
		modificationSites=data.getModificationSites();
		numberOfModifSites=data.getNumberOfModifSites();
		compact=data.isCompact();
		setAsCompact=data.isSetAsCompact();
		toDelete=data.isToDelete();
		stoichoimetryForReaction=data.getStoichoimetryForReaction();
		
	}

	public String getInitialConcentration() {
		if(initialConcentration.trim().equals("0")){
			initialConcentration="0.0";
		}
		return initialConcentration;
	}

	public void setInitialConcentration(String initialConcentration) {
		if(initialConcentration!=null){
			this.initialConcentration = initialConcentration.trim();
			putProperty("initConc",initialConcentration);
		}
		
	}

	public String getMoleculeState() {
		return moleculeState;
	}

	public void setMoleculeState(String moleculeState) {
		this.moleculeState = moleculeState;
		putProperty("state",moleculeState);
	}

	public String getCompartment() {
		if(compartment.startsWith("::")){
			compartment=compartment.substring(compartment.lastIndexOf(":")+1);
		}
		return compartment;
	}

	public void setCompartment(String volume) {
		this.compartment = volume;
		putProperty("compartment",volume);
	}

	public int getMultimerCardinality() {
		return multimerCardinality;
	}

	public void setMultimerCardinality(int cardinality) {
		this.multimerCardinality = cardinality;
	}

	
	@Override
	public void valueChanged(GraphSelectionEvent e) {
		// TODO Auto-generated method stub
		
	}

	public boolean isModulator() {
		return modulator;
	}

	public void setModulator(boolean modulator) {
		if(isReactant() || isProduct()){
			setMoreRolePerReaction(true);
		}
		this.modulator = modulator;
	}

	public ArrayList<ContainingMolecule> getContainingMolecules() {
		return containingMolecules;
	}

	public void setContainingMolecules(
			ArrayList<ContainingMolecule> containingMolecules) {
		this.containingMolecules = containingMolecules;
	}

	public Color getColor() {
		return color;
	}

	public void setColor(Color color) {
		this.color = color;
	}

	public BiochamGraph getGraph() {
		return graph;
	}

	
	public void setGraph(BiochamGraph graph) {
		this.graph = graph;
	}

	public UUID getId() {
		return id;
	}

	public void setId(UUID id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getRepresentingName() {
		if(representingName==null){
			String nm1=getName();
			if(getMultimerCardinality()>1 && nm1.contains("-")){
				String tmpStr=getName().substring(getName().lastIndexOf("-")+1,getName().length());	
				nm1=getName().substring(0,getName().indexOf(tmpStr))+tmpStr;
			}
			setRepresentingName(nm1);	
		}
		if(representingName!=null){
			if(representingName.contains("~")){
				representingName=representingName.substring(0,representingName.indexOf("~"));
			}
		}
		return representingName;
	}

	public void setRepresentingName(String representingName) {
		this.representingName = representingName;
	}

	public String getMoleculeType() {
		String ss=this.getName();
		DefaultGraphCell c=GraphUtilities.getCellByName(this.getGraph(),ss);		
		if(c!=null){		
			moleculeType=GraphUtilities.getCellType(c);			
		}else{
			c=GraphUtilities.getCellById(this.getGraph(),this.getId());
			if(c!=null){		
				moleculeType=GraphUtilities.getCellType(c);			
			}else{
				moleculeType="Macromolecule";	
			}
		}
		return moleculeType;
	}

	public void setMoleculeType(String moleculeType) {
		this.moleculeType = moleculeType;
	}

	public Position getPosition() {
		return position;
	}

	public void setPosition(Position position) {
		this.position = position;
	}

	public MoleculeSize getSize() {
		return size;
	}

	public void setSize(MoleculeSize size) {
		this.size = size;
	}

	public int getNumberOfConnections() {
		return numberOfConnections;
	}

	public void setNumberOfConnections(int numberOfConnections) {
		this.numberOfConnections = numberOfConnections;
	}

	public ArrayList<String> getInvolvedInReactions() {
		int siz=involvedInReactions.size();
		return involvedInReactions;
	}

	public void setInvolvedInReactions(ArrayList<String> involvedInReactions) {
		this.involvedInReactions = involvedInReactions;
	}

	public boolean isProduct() {
		return product;
	}

	public void setProduct(boolean product) {
		if(isReactant() || isModulator()){
			setMoreRolePerReaction(true);
		}
		this.product = product;
	}

	public boolean isReactant() {
		return reactant;
	}

	public void setReactant(boolean reactant) {
		if(isProduct() || isModulator()){
			setMoreRolePerReaction(true);
		}
		this.reactant = reactant;
	}

	public boolean isResponsable() {
		return responsable;
	}

	public void setResponsable(boolean responsable) {
		this.responsable = responsable;
	}

	public boolean isCopy() {
		return copy;
	}

	public void setCopy(boolean copy) {
		this.copy = copy;
	}

	public int getCopyInstance() {
		return copyInstance;
	}

	public void setCopyInstance(int copyInstance) {
		this.copyInstance = copyInstance;
	}

	public int getNumberOfModifSites() {
		if(numberOfModifSites==0){					
			String m=getModificationSites();
			if(m!=null){
				StringTokenizer st=new StringTokenizer(m,",");
				int tkns=st.countTokens();
				while(st.hasMoreTokens()){
					numberOfModifSites++;
					st.nextToken();
				
				}
				st=null;
			}
		}
		return numberOfModifSites;
	}

	public void setNumberOfModifSites(int numberOfModifSites) {
		this.numberOfModifSites = numberOfModifSites;
	}

	public void applySize() {
		
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();
		DefaultGraphCell cell=GraphUtilities.getCellById(graph,id);
		Rectangle2D bnds=BiochamGraphConstants.getBounds(cell.getAttributes());
		BiochamGraphConstants.setBounds(attributeMap1,new Rectangle2D.Double(bnds.getX(),bnds.getY(),getSize().getWidth(),getSize().getHeight()));
		nested.put(cell, attributeMap1 );
		graph.getGraphLayoutCache().edit(nested, null, null, null);
		Utils.debugMsg("\n\n****after="+getSize().width);
	}

	public boolean isCompact() {
		return compact;
	}

	public void setCompact(boolean compact) {
		this.compact = compact;
	}

	public boolean isSetAsCompact() {
		return setAsCompact;
	}

	public void setSetAsCompact(boolean setAsCompact) {
		this.setAsCompact = setAsCompact;
	}

	public boolean isToDelete() {
		return toDelete;
	}

	public void setToDelete(boolean toDelete) {
		this.toDelete = toDelete;
	}

	public String getStoichoimetryForReaction() {
		return stoichoimetryForReaction;
	}
	public int getStoichoimetryIntegerForReaction() {
		if(stoichoimetryForReaction!=null){
			if(stoichoimetryForReaction!=""){
				try{
					int st=0;
					try{
						st=Integer.parseInt(stoichoimetryForReaction);
					}catch(Exception e){
						st=0;
					}
					return st;
				}catch(Exception e){
					return 0;
				}
			}
		}
		return 0;
	}

	public void setStoichoimetryForReaction(String stoichoimetryForReaction) {
		this.stoichoimetryForReaction = stoichoimetryForReaction;
	}

	public void setStoichoimetryForReaction(int reactionStoich) {
		stoichoimetryForReaction=Integer.toString(reactionStoich);
		
	}

	public Rectangle2D getBounds() {
		return bounds;
	}

	public void setBounds(Rectangle2D bounds) {
		this.bounds = bounds;
	}

	public boolean isSetByAlgo() {
		return setByAlgo;
	}

	public void setSetByAlgo(boolean setByAlgo) {
		this.setByAlgo = setByAlgo;
	}

	public int getModulatorPosition() {
		return modulatorPosition;
	}

	public void setModulatorPosition(int modulatorPosition) {
		this.modulatorPosition = modulatorPosition;
	}

	public boolean isMoreRolePerReaction() {
		return moreRolePerReaction;
	}

	public void setMoreRolePerReaction(boolean moreRolePerReaction) {
		this.moreRolePerReaction = moreRolePerReaction;
	}
	
}
