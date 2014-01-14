package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.Edge;

import java.awt.geom.Point2D;
import java.util.Map;

public class JGraphpadGraphConstants {
	/**
	028:             * Shared routing instance for parallel routing.
	029:             */
	          //  public final static Edge.Routing ROUTING_PARALLEL = JGraphpadParallelEdgeRouter.sharedInstance;
	
	            /**
	033:             * Shared routing instance for parallel spline routing.
	034:             */
	            public final static Edge.Routing ROUTING_PARALLELSPLINE = GraphParallelSplineRouter.sharedInstance;
	
	            /**
	038:             * Key for the <code>stretchImage</code> attribute. This special attribute
	039:             * contains a Boolean instance indicating whether the background image
	040:             * should be stretched.
	041:             */
	            public final static String STRETCHIMAGE = "stretchImage";
	
	            /**
	045:             * Key for the <code>groupResize</code> attribute. This special attribute
	046:             * contains a Boolean instance indicating if the group should be resized
	047:             * when it is collapsed. This is usually set to true before the first
	048:             * collapse and then removed.
	049:             */
	            public final static String GROUPRESIZE = "groupResize";
	
	            /**
	053:             * Key for the <code>groupReposition</code> attribute. This special
	054:             * attribute contains a Boolean instance indicating if the collapsed group
	055:             * should be moved to the top left corner of it's child area when it is
	056:             * collapsed.
	057:             */
	            public final static String GROUPREPOSITION = "groupReposition";
	
	            /**
	061:             * Key for the <code>vertexShape</code> attribute. This special attribute
	062:             * contains an Integer instance indicating which shape should be drawn by
	063:             * the renderer.
	064:             */
	            public final static String VERTEXSHAPE = "vertexShape";
	
	            /**
	068:             * Key for the <code>sourcePortOffset</code> attribute. This special
	069:             * attribute contains a Point2D instance indicating the relative position of
	070:             * a port in its parents coordinate space seen from a specific edge.
	071:             */
	          public final static String SOURCEPORTOFFSET = "sourcePortOffset";
	
	            /**
	075:             * Key for the <code>targetPortOffset</code> attribute. This special
	076:             * attribute contains a Point2D instance indicating the relative position of
	077:             * a port in its parents coordinate space seen from a specific edge.
	078:             */
	            public final static String TARGETPORTOFFSET = "targetPortOffset";
	
	            /**
	082:             * Returns true if stretchImage in this map is true. Default is false.
	083:             */
	            public static final boolean isStretchImage(Map map) {
	                Boolean boolObj = (Boolean) map.get(STRETCHIMAGE);
	                if (boolObj != null)
	                    return boolObj.booleanValue();
	                return false;
	            }
	
	            /**
	092:             * Sets stretchImage in the specified map to the specified value.
	093:             */
	            public static final void setStretchImage(Map map,
	                    boolean stretchImage) {
	                map.put(STRETCHIMAGE, new Boolean(stretchImage));
	            }
	            /**
	100:             * Returns true if groupResize in this map is true. Default is false.
	101:             */
	            public static final boolean isGroupResize(Map map) {
	                Boolean boolObj = (Boolean) map.get(GROUPRESIZE);
	                if (boolObj != null)
	                    return boolObj.booleanValue();
	                return false;
	            }
	
	            /**
	110:             * Sets groupResize in the specified map to the specified value.
	111:             */
	            public static final void setGroupResize(Map map,
	                    boolean stretchImage) {
	                map.put(GROUPRESIZE, new Boolean(stretchImage));
	            }
	
	            /**
	118:             * Returns true if groupReposition in this map is true. Default is true.
	119:             */
	            public static final boolean isGroupReposition(Map map) {
	                Boolean boolObj = (Boolean) map.get(GROUPREPOSITION);
	                if (boolObj != null)
	                    return boolObj.booleanValue();
	                return true;
	            }
	
	            /**
	128:             * Sets groupReposition in the specified map to the specified value.
	129:             */
	           public static final void setGroupReposition(Map map,
	                    boolean stretchImage) {
	                map.put(GROUPREPOSITION, new Boolean(stretchImage));
	            }
	
	            /**
	136:             * Sets vertexShape in the specified map to the specified value.
	137:             */
	            public static final void setVertexShape(Map map, int shape) {
	                map.put(VERTEXSHAPE, new Integer(shape));
	            }
	
	            /**
	143:             * Returns vertexShape from the specified map.
	144:             */
	            public static final int getVertexShape(Map map) {
	                Integer intObj = (Integer) map.get(VERTEXSHAPE);
	                if (intObj != null)
	                    return intObj.intValue();
	                return 0;
	            }
	
	            /**
	153:             * Sets sourcePortOffset in the specified map to the specified value.
	154:             */
	            public static final void setSourcePortOffset(Map map, Point2D offset) {
	                map.put(SOURCEPORTOFFSET, offset);
	            }
	
	            /**
	160:             * Returns sourcePortOffset from the specified map.
	161:             */
	            public static final Point2D getSourcePortOffset(Map map) {
	                return (Point2D) map.get(SOURCEPORTOFFSET);
	            }
	
	            /**
	167:             * Sets targetPortOffset in the specified map to the specified value.
	168:             */
	            public static final void setTargetPortOffset(Map map, Point2D offset) {
	                map.put(TARGETPORTOFFSET, offset);
	            }
	            /**
	174:             * Returns targetPortOffset from the specified map.
	175:             */
	            public static final Point2D getTargetPortOffset(Map map) {
	                return (Point2D) map.get(TARGETPORTOFFSET);
	            }
	            /**
	181:             * Returns the shared instance of the parallel routing.
	182:             */
	            public static Edge.Routing getParallelEdgeRouting() {
	                return null;
	            }
	
	            /**
	188:             * Returns the shared instance of the parallel spline routing.
	189:             */
	            public static Edge.Routing getParallelSplineRouting() {
	                return ROUTING_PARALLELSPLINE;
	            }
	
}


