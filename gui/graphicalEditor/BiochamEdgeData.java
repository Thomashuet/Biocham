package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.VertexView;

import java.awt.Color;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Map;
import java.util.UUID;







public class BiochamEdgeData {

	
	ArrayList<BiochamEnitity> reactants, products,modulators1,modulators2;	
	DefaultGraphCell modulator1, reversibleModulator;	
	int stoichiometry=0;
	int reversibilityType;
	boolean reversible=false;
	String kinetics="1.0";
	String name="";	
	Map properties;
	BiochamGraph graph;
	String compartment;
	ArrayList<UUID> molecules;
	Position position;
	String type;
	Color color;
	String orientation;
	String leftSide, rightSide;
	ReactionProperties reactionProperties;
	boolean hasOpposite=false;
	Point2D firstPoint, lastPoint;
	VertexView drawingInstance;
	
	public BiochamEdgeData(BiochamGraph g,ArrayList<UUID> containingMolecules){
		this(null,g,containingMolecules);
	}	
	public BiochamEdgeData(BiochamEdgeData data){
		this(null,data.getGraph(),data.getMolecules());
		setKinetics(data.getKinetics());
		this.setColor(data.getColor());
		this.setCompartment(data.getCompartment());
		this.setModulators1(data.getModulators1());
		this.setModulators2(data.getModulators2());
		this.setName(data.getName());
		this.setOrientation(data.getOrientation());
		this.setPosition(data.getPosition());
		this.setProducts(data.getProducts());
		this.setReactants(data.getReactants());
		this.setReversible(data.isReversible());
		data.setStoichiometry(data.getStoichiometry());
		data.setType(data.getType());
	}	
	public BiochamEdgeData(Map props,BiochamGraph g,ArrayList<UUID> containingMolecules){		
		properties=new Hashtable(6);	
		graph=g;
		molecules=containingMolecules;
		reactionProperties=new ReactionProperties();
		//position=new Vector<Position>();
	}
	public BiochamEdgeData(BiochamGraph g){
		this(null,g,null);
	}	
	
	
	
	
	public String getKinetics() {
		if(kinetics!=null){
			if(kinetics.contains("Type")){
				return "";
			}else{
				return kinetics;
			}
		}
		return "";
		
	}

	public void setKinetics(String kinetics) {
		this.kinetics = kinetics;
	}

	//ArrayList<String> rcts, pcts,mdts;
	public ArrayList<String> getReactantsAsStrings() {
		ArrayList<String> rcts=new ArrayList<String>(reactants.size());
		for(int i=0;i<reactants.size();i++){
			rcts.add(reactants.get(i).getName());
		}
		return rcts;
	}
	public ArrayList<String> getProductsAsStrings() {
		ArrayList<String> rcts=new ArrayList<String>(products.size());
		for(int i=0;i<products.size();i++){
			rcts.add(products.get(i).getName());
		}
		return rcts;
	}
	public ArrayList<String> getModulators1AsStrings() {
		ArrayList<String> rcts=new ArrayList<String>(modulators1.size());
		for(int i=0;i<modulators1.size();i++){
			rcts.add(modulators1.get(i).getName());
		}
		return rcts;
	}
	public ArrayList<String> getModulators2AsStrings() {
		if(modulators2!=null){
			ArrayList<String> rcts=new ArrayList<String>(modulators2.size());
			for(int i=0;i<modulators2.size();i++){
				rcts.add(modulators2.get(i).getName());
			}
			return rcts;
		}else{
			return null;
		}
		
	}
		
	public ArrayList<String> getAllModulatorsAsStrings() {
	
		ArrayList<String> rcts=new ArrayList<String>();
		
		if(modulators1!=null && modulators2!=null){
			for(int i=0;i<modulators1.size();i++){
				rcts.add(modulators1.get(i).getName());
			}		
			for(int i=0;i<modulators2.size();i++){
				rcts.add(modulators2.get(i).getName());
			}
		}else if(modulators1!=null){
			for(int i=0;i<modulators1.size();i++){
				rcts.add(modulators1.get(i).getName());
			}
		}else if(modulators2!=null){
			for(int i=0;i<modulators2.size();i++){
				rcts.add(modulators2.get(i).getName());
			}
		}		
		return rcts;
	}
	
	public ArrayList<BiochamEnitity> getProducts() {
		if(products==null){
			products=new ArrayList<BiochamEnitity>();
		}
		int siz=products.size();
		return products;
	}

	public void setProducts(ArrayList<BiochamEnitity> products) {
		this.products = products;
	}

	public ArrayList<BiochamEnitity> getReactants() {
		if(reactants==null){
			reactants=new ArrayList<BiochamEnitity>();
		}		
		return reactants;
	}

	public void setReactants(ArrayList<BiochamEnitity> reactants) {
		this.reactants = reactants;
	}

	public ArrayList<BiochamEnitity> getModulators1() {
		if(modulators1==null){
			modulators1=new ArrayList<BiochamEnitity>();
		}
		return modulators1;
	}
	
	public void setModulators1(ArrayList<BiochamEnitity> modulators) {
		this.modulators1 = modulators;
	}
	public ArrayList<BiochamEnitity> getModulators2() {
		if(modulators2==null){
			modulators2=new ArrayList<BiochamEnitity>();
		}
		return modulators2;
	}
	
	public void setModulators2(ArrayList<BiochamEnitity> modulators) {
		this.modulators2 = modulators;
	}
	
	public int getStoichiometry() {
		return stoichiometry;
	}

	public void setStoichiometry(int stoichiometry) {
		this.stoichiometry = stoichiometry;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		reactionProperties.setActualName(name);
		this.name = name;
	}

	public int getReversibilityType() {
		return reversibilityType;
	}

	public void setReversibilityType(int reversibilityType) {
		this.reversibilityType = reversibilityType;
	}

	public boolean isReversible() {
		return reversible;
	}

	public void setReversible(boolean reversible) {
		this.reversible = reversible;
	}

	public DefaultGraphCell getModulator1() {
		return modulator1;
	}

	public void setModulator1(DefaultGraphCell modulator1) {
		this.modulator1 = modulator1;
	}

	public DefaultGraphCell getReversibleModulator() {
		return reversibleModulator;
	}

	public void setReversibleModulator(DefaultGraphCell reversibleModulator) {
		this.reversibleModulator = reversibleModulator;
	}
	
	public String toString(){
		return "";
	}
	
	
	
	
	
	
	public Object getProperty(Object key){
		return properties.get(key);
	}
	
	public Object putProperty(Object key, Object value){
		if(value!=null){
			return properties.put(key,value);
		}
		return null;
	}
	
	public Map getProperties(){
		return properties;
	}	
	public void setProperties(Map props){
		this.properties=props;
	}
	public BiochamGraph getGraph() {
		return graph;
	}
	public void setGraph(BiochamGraph graph) {
		this.graph = graph;
	}
	public String getCompartment() {
		return compartment;
	}
	public void setCompartment(String compartment) {
		this.compartment = compartment;
	}
	public ArrayList<UUID> getMolecules() {
		return molecules;
	}
	public void setMolecules(ArrayList<UUID> molecules) {
		this.molecules = molecules;
	}
	public Position getPosition() {
		return position;
	}
	public void setPosition(Position position) {
		this.position = position;
	}
	public String getType() {
		return type;
	}
	public void setType(String type) {
		this.type = type;
	}
	public Color getColor() {
		return color;
	}
	public void setColor(Color color) {
		this.color = color;
	}
	public String getOrientation() {
		return orientation;
	}
	public void setOrientation(String orientation) {
		this.orientation = orientation;
	}
	public ReactionProperties getReactionProperties() {
		return reactionProperties;
	}
	public void setReactionProperties(ReactionProperties reactionProperties) {
		this.reactionProperties = reactionProperties;
	}
	public boolean isHasOpposite() {
		return hasOpposite;
	}
	public void setHasOpposite(boolean hasOpposite) {
		this.hasOpposite = hasOpposite;
	}
	public Point2D getFirstPoint() {
		return firstPoint;
	}
	public void setFirstPoint(Point2D firstPoint) {
		this.firstPoint = firstPoint;
	}
	public Point2D getLastPoint() {
		return lastPoint;
	}
	public void setLastPoint(Point2D lastPoint) {
		this.lastPoint = lastPoint;
	}
	public void setDrawingInstance(IntermediateVertexNode node) {
		drawingInstance=node;
		
	}
	
	
}
