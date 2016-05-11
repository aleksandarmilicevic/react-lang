package react.robot

import net.jcazevedo.moultingyaml._
import dzufferey.smtlib._

case class ComponentDataModel(
    connections: List[ConnectionDef],
    constants: Map[String, Double],
    constraints: List[Formula],
    interfaces: Map[String, InterfaceDef],
    parameters: Map[String, Formula],
    subcomponents: Map[String, SubcomponentDef]
)

case class ConnectionDef(source: (String, String), // subcomponent, interface
                         dest: (String, String),   // subcomponent, interface
                         parameters: Map[String, Formula])

case class InterfaceDef(interface: String, subcomponent: String)

case class SubcomponentDef(clazz: String, constants: Map[String,Double], parameters: Map[String, Formula])


//
// The corresponding YAML protocol
//

object ComponentDataModelProtocol extends DefaultYamlProtocol {

  implicit object FormulaFormat extends YamlFormat[Formula] {
    def write(c: Formula) = c match {
      case Literal(b: Boolean) => YamlBoolean(b)
      case Literal(l: Int)     => YamlNumber(l)
      case Literal(l: Long)    => YamlNumber(l)
      case Literal(l: Double)  => YamlNumber(l)
      case _ => YamlString(TextPrinter.toString(c))
    }
    def read(value: YamlValue) = value match {
      case YamlBoolean(b)        => Literal(b)
      case YamlNumber(l: Long)   => Literal(l)
      case YamlNumber(l: Int)    => Literal(l)
      case YamlNumber(l: Double) => Literal(l)
      case YamlString(s) =>
        ???
      case _ =>
        deserializationError("Formula expected")
    }
  }
  
  implicit object ConnectionFormat extends YamlFormat[ConnectionDef] {
    def write(c: ConnectionDef) = {
      YamlArray(c.source.toYaml, c.dest.toYaml, c.parameters.toYaml)
    }
    def read(value: YamlValue) = value match {
      case YamlArray(Vector(s,d,parameters)) =>
        ConnectionDef(s.convertTo[(String, String)],
                      d.convertTo[(String, String)],
                      parameters.convertTo[Map[String,Formula]])
      case _ =>
        deserializationError("ConnectionDef expected")
    }
  }

  implicit object InterfaceFormat extends YamlFormat[InterfaceDef] {
    def write(c: InterfaceDef) = {
      YamlObject(
        YamlString("interface")    -> YamlString(c.interface),
        YamlString("subcomponent") -> YamlString(c.subcomponent)
      )
    }
    def read(value: YamlValue) = {
      value.asYamlObject.getFields(
        YamlString("interface"),
        YamlString("subcomponent")
      ) match {
        case Seq(YamlString(i), YamlString(s)) =>
          InterfaceDef(i, s)
        case _ =>
          deserializationError("InterfaceDef expected")
      }
    }
  }

  implicit object SubcomponentFormat extends YamlFormat[SubcomponentDef] {
    def write(c: SubcomponentDef) = {
      YamlObject(
        YamlString("class")      -> YamlString(c.clazz),
        YamlString("constants")  -> c.constants.toYaml,
        YamlString("parameters") -> c.parameters.toYaml
      )
    }
    def read(value: YamlValue) = {
      value.asYamlObject.getFields(
        YamlString("class"),
        YamlString("constants"),
        YamlString("parameters")
      ) match {
        case Seq(YamlString(c), csts, params) =>
          SubcomponentDef(c,
                          csts.convertTo[Map[String,Double]],
                          params.convertTo[Map[String,Formula]])
        case _ =>
          deserializationError("SubcomponentDef expected")
      }
    }
  }

  implicit object ComponentFormat extends YamlFormat[ComponentDataModel] {
    def write(c: ComponentDataModel) = {
      YamlObject(
        YamlString("connections")   -> c.connections.toYaml,
        YamlString("constants")     -> c.constants.toYaml,
        YamlString("constraints")   -> c.constraints.toYaml,
        YamlString("interface")     -> c.interfaces.toYaml,
        YamlString("parameters")    -> c.parameters.toYaml,
        YamlString("subcomponents") -> c.subcomponents.toYaml
      )
    }

    def read(value: YamlValue) = {
      value.asYamlObject.getFields(
        YamlString("connections"),
        YamlString("constants"),
        YamlString("constraints"),
        YamlString("interface"),
        YamlString("parameters"),
        YamlString("subcomponents")
      ) match {
        case Seq(
          connections,
          constants,
          constraints,
          interface,
          parameters,
          subcomponents
        ) =>
          ComponentDataModel(
            connections.convertTo[List[ConnectionDef]],
            constants.convertTo[Map[String,Double]],
            constraints.convertTo[List[Formula]],
            interface.convertTo[Map[String,InterfaceDef]],
            parameters.convertTo[Map[String,Formula]],
            subcomponents.convertTo[Map[String,SubcomponentDef]]
          )
        case _ =>
          deserializationError("ComponentDataModel expected")
      }
    }
  }

}
