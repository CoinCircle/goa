package service

import (
	"fmt"
	"strings"

	"goa.design/goa/codegen"
	"goa.design/goa/design"
)

// Services holds the data computed from the design needed to generate the code
// of the services.
var Services = make(ServicesData)

type (
	// ServicesData encapsulates the data computed from the service designs.
	ServicesData map[string]*Data

	// Data contains the data used to render the code related to a
	// single service.
	Data struct {
		// Name is the service name.
		Name string
		// Description is the service description.
		Description string
		// StructName is the service struct name.
		StructName string
		// VarName is the service variable name (first letter in lowercase).
		VarName string
		// PkgName is the name of the package containing the generated
		// service code.
		PkgName string
		// Methods lists the service interface methods.
		Methods []*MethodData
		// UserTypes lists the types definitions that the service
		// depends on.
		UserTypes []*UserTypeData
		// ErrorTypes lists the error types definitions that the service
		// depends on.
		ErrorTypes []*UserTypeData
		// Errors list the information required to generate error init
		// functions.
		ErrorInits []*ErrorInitData
		// ExpandedResult contains the data to generate the expanded result type.
		ExpandedTypes []*ExpandedTypeData
		// Scope initialized with all the service types.
		Scope   *codegen.NameScope
		Helpers []*codegen.TransformFunctionData
	}

	// ErrorInitData describes an error returned by a service method of type
	// ErrorResult.
	ErrorInitData struct {
		// Name is the name of the init function.
		Name string
		// Description is the error description.
		Description string
		// ErrName is the name of the error.
		ErrName string
		// TypeName is the error struct type name.
		TypeName string
		// TypeRef is the reference to the error type.
		TypeRef string
		// Temporary indicates whether the error is temporary.
		Temporary bool
		// Timeout indicates whether the error is due to timeouts.
		Timeout bool
	}

	// MethodData describes a single service method.
	MethodData struct {
		// Name is the method name.
		Name string
		// Description is the method description.
		Description string
		// VarName is the Go method name.
		VarName string
		// Payload is the name of the payload type if any,
		Payload string
		// PayloadDef is the payload type definition if any.
		PayloadDef string
		// PayloadRef is a reference to the payload type if any,
		PayloadRef string
		// PayloadDesc is the payload type description if any.
		PayloadDesc string
		// PayloadEx is an example of a valid payload value.
		PayloadEx interface{}
		// Result is the name of the result type if any.
		Result string
		// ResultDef is the result type definition if any.
		ResultDef string
		// ResultRef is the reference to the result type if any.
		ResultRef string
		// ResultDesc is the result type description if any.
		ResultDesc string
		// ResultEx is an example of a valid result value.
		ResultEx interface{}
		// Errors list the possible errors defined in the design if any.
		Errors []*ErrorInitData
		// ExpandedResultType is data containing the expanded result type.
		ExpandedResult *ExpandedTypeData
	}

	// UserTypeData contains the data describing a data type.
	UserTypeData struct {
		// Name is the type name.
		Name string
		// VarName is the corresponding Go type name.
		VarName string
		// Description is the type human description.
		Description string
		// Def is the type definition Go code.
		Def string
		// Ref is the reference to the type.
		Ref string
		// Type is the underlying type.
		Type design.UserType
	}

	// FieldData contains the data needed to render a single field.
	FieldData struct {
		// Name is the name of the attribute.
		Name string
		// VarName is the name of the Go type field.
		VarName string
		// TypeRef is the reference to the field type.
		TypeRef string
		// Required is true if the field is required.
		Required bool
		// DefaultValue is the payload attribute default value if any.
		DefaultValue interface{}
	}

	// ExpandedTypeData contains the data to generate expanded types to
	// deal with view rendering.
	ExpandedTypeData struct {
		// Name is the type name.
		Name string
		// VarName is the corresponding Go type name.
		VarName string
		// Description is the type human description.
		Description string
		// Def is the type definition Go code.
		Def string
		// FullRef is the complete reference to the type (including pkg name)
		FullRef string
		// Ref is the reference to the type.
		Ref string
		// ResultName is the name of the result type.
		ResultName string
		// ResultRef is the reference to result type being dealt by the renderer.
		ResultRef string
		// Validate is the validation code.
		Validate string
		// Views is the defined views on a result type
		Views []*RenderData
		// Type is the underlying expanded type.
		Type design.UserType
		// SrcType is the source type from which the expanded type is created.
		SrcType design.UserType
		// Helpers is the marshal/unmarshal helpers.
		Helpers      []*codegen.TransformFunctionData
		IsCollection bool
	}

	// RenderData contains the data to create from/convert to a result type
	// with a specific view.
	RenderData struct {
		// View is the view name.
		View string
		// Description is the view description.
		Description string
		// ToResult is the function name to convert expand type to a result type
		// based on a view.
		ToResult string
		// FromResult is the function name to convert a result type to an
		// expanded type based on a view.
		FromResult string
		// ToResultCode is the code for ToResult function.
		ToResultCode string
		// FromResultCode is the code for FromResult function.
		FromResultCode string
		// Ref is the reference to the type
		Ref string
	}
)

// Get retrieves the data for the service with the given name computing it if
// needed. It returns nil if there is no service with the given name.
func (d ServicesData) Get(name string) *Data {
	if data, ok := d[name]; ok {
		return data
	}
	service := design.Root.Service(name)
	if service == nil {
		return nil
	}
	d[name] = d.analyze(service)
	return d[name]
}

// Method returns the service method data for the method with the given name,
// nil if there isn't one.
func (s *Data) Method(name string) *MethodData {
	for _, m := range s.Methods {
		if m.Name == name {
			return m
		}
	}
	return nil
}

// analyze creates the data necessary to render the code of the given service.
// It records the user types needed by the service definition in userTypes.
func (d ServicesData) analyze(service *design.ServiceExpr) *Data {
	var (
		scope      *codegen.NameScope
		pkgName    string
		types      []*UserTypeData
		errTypes   []*UserTypeData
		errorInits []*ErrorInitData
		expTypes   []*ExpandedTypeData
		helpers    []*codegen.TransformFunctionData
		seenErrors map[string]struct{}
		seen       map[string]struct{}
	)
	{
		scope = codegen.NewNameScope()
		pkgName = scope.Unique(service, strings.ToLower(codegen.Goify(service.Name, false)), "svc")
		seen = make(map[string]struct{})
		seenErrors = make(map[string]struct{})
		for _, e := range service.Methods {
			// Create user type for raw object payloads
			if _, ok := e.Payload.Type.(*design.Object); ok {
				e.Payload.Type = &design.UserTypeExpr{
					AttributeExpr: design.DupAtt(e.Payload),
					TypeName:      fmt.Sprintf("%sPayload", codegen.Goify(e.Name, true)),
				}
			}

			if ut, ok := e.Payload.Type.(design.UserType); ok {
				seen[ut.Name()] = struct{}{}
			}

			// Create user type for raw object results
			if _, ok := e.Result.Type.(*design.Object); ok {
				e.Result.Type = &design.UserTypeExpr{
					AttributeExpr: design.DupAtt(e.Result),
					TypeName:      fmt.Sprintf("%sResult", codegen.Goify(e.Name, true)),
				}
			}

			if ut, ok := e.Result.Type.(design.UserType); ok {
				seen[ut.Name()] = struct{}{}
			}
		}
		recordError := func(er *design.ErrorExpr) {
			errTypes = append(errTypes, collectTypes(er.AttributeExpr, seen, scope)...)
			if er.Type == design.ErrorResult {
				if _, ok := seenErrors[er.Name]; ok {
					return
				}
				seenErrors[er.Name] = struct{}{}
				errorInits = append(errorInits, buildErrorInitData(er, scope))
			}
		}
		for _, er := range service.Errors {
			recordError(er)
		}
		for _, m := range service.Methods {
			patt := m.Payload
			if ut, ok := patt.Type.(design.UserType); ok {
				patt = ut.Attribute()
			}
			types = append(types, collectTypes(patt, seen, scope)...)
			ratt := m.Result
			if ut, ok := ratt.Type.(design.UserType); ok {
				ratt = ut.Attribute()
			}
			types = append(types, collectTypes(ratt, seen, scope)...)
			for _, er := range m.Errors {
				recordError(er)
			}
		}
	}

	var (
		methods []*MethodData
	)
	{
		methods = make([]*MethodData, len(service.Methods))
		for i, e := range service.Methods {
			m := buildMethodData(e, pkgName, scope)
			if rt, ok := e.Result.Type.(*design.ResultTypeExpr); ok && len(rt.Views) > 1 {
				seen = make(map[string]struct{})
				expResult := buildExpandedResultType(e.Result, scope)
				if expResult != nil {
					types := collectExpandedTypes(e.Result, expResult, scope, seen)
					m.ExpandedResult = types[0]
					for _, i := range types {
						var found bool
						for _, j := range expTypes {
							if i.VarName == j.VarName {
								found = true
								break
							}
						}
						if !found {
							expTypes = append(expTypes, i)
						}
					}
				}
			}
			methods[i] = m
		}
	}

	var (
		desc string
	)
	{
		desc = service.Description
		if desc == "" {
			desc = fmt.Sprintf("Service is the %s service interface.", service.Name)
		}
	}

	data := &Data{
		Name:          service.Name,
		Description:   desc,
		VarName:       codegen.Goify(service.Name, false),
		StructName:    codegen.Goify(service.Name, true),
		PkgName:       pkgName,
		Methods:       methods,
		UserTypes:     types,
		ErrorTypes:    errTypes,
		ErrorInits:    errorInits,
		ExpandedTypes: expTypes,
		Helpers:       helpers,
		Scope:         scope,
	}
	d[service.Name] = data

	return data
}

// collectTypes recurses through the attribute to gather all user types and
// records them in userTypes.
func collectTypes(at *design.AttributeExpr, seen map[string]struct{}, scope *codegen.NameScope) (data []*UserTypeData) {
	if at == nil || at.Type == design.Empty {
		return
	}
	collect := func(at *design.AttributeExpr) []*UserTypeData { return collectTypes(at, seen, scope) }
	switch dt := at.Type.(type) {
	case design.UserType:
		if _, ok := seen[dt.Name()]; ok {
			return nil
		}
		data = append(data, &UserTypeData{
			Name:        dt.Name(),
			VarName:     scope.GoTypeName(at),
			Description: dt.Attribute().Description,
			Def:         scope.GoTypeDef(dt.Attribute(), true),
			Ref:         scope.GoTypeRef(at),
			Type:        dt,
		})
		seen[dt.Name()] = struct{}{}
		data = append(data, collect(dt.Attribute())...)
	case *design.Object:
		for _, nat := range *dt {
			data = append(data, collect(nat.Attribute)...)
		}
	case *design.Array:
		data = append(data, collect(dt.ElemType)...)
	case *design.Map:
		data = append(data, collect(dt.KeyType)...)
		data = append(data, collect(dt.ElemType)...)
	}
	return
}

// buildErrorInitData creates the data needed to generate code around endpoint error return values.
func buildErrorInitData(er *design.ErrorExpr, scope *codegen.NameScope) *ErrorInitData {
	_, temporary := er.AttributeExpr.Metadata["goa:error:temporary"]
	_, timeout := er.AttributeExpr.Metadata["goa:error:timeout"]
	return &ErrorInitData{
		Name:        fmt.Sprintf("Make%s", codegen.Goify(er.Name, true)),
		Description: er.Description,
		ErrName:     er.Name,
		TypeName:    scope.GoTypeName(er.AttributeExpr),
		TypeRef:     scope.GoTypeRef(er.AttributeExpr),
		Temporary:   temporary,
		Timeout:     timeout,
	}
}

// buildMethodData creates the data needed to render the given endpoint. It
// records the user types needed by the service definition in userTypes.
func buildMethodData(m *design.MethodExpr, svcPkgName string, scope *codegen.NameScope) *MethodData {
	var (
		varName     string
		desc        string
		payloadName string
		payloadDef  string
		payloadRef  string
		payloadDesc string
		payloadEx   interface{}
		resultName  string
		resultDef   string
		resultRef   string
		resultDesc  string
		resultEx    interface{}
		errors      []*ErrorInitData
	)
	{
		varName = codegen.Goify(m.Name, true)
		if m.Payload.Type != design.Empty {
			payloadName = scope.GoTypeName(m.Payload)
			payloadRef = scope.GoTypeRef(m.Payload)
			if dt, ok := m.Payload.Type.(design.UserType); ok {
				payloadDef = scope.GoTypeDef(dt.Attribute(), true)
			}
			payloadDesc = m.Payload.Description
			if payloadDesc == "" {
				payloadDesc = fmt.Sprintf("%s is the payload type of the %s service %s method.",
					payloadName, m.Service.Name, m.Name)
			}
			payloadEx = m.Payload.Example(design.Root.API.Random())
		}
		if m.Result.Type != design.Empty {
			resultName = scope.GoTypeName(m.Result)
			resultRef = scope.GoTypeRef(m.Result)
			if dt, ok := m.Result.Type.(design.UserType); ok {
				resultDef = scope.GoTypeDef(dt.Attribute(), true)
			}
			resultDesc = m.Result.Description
			if resultDesc == "" {
				resultDesc = fmt.Sprintf("%s is the result type of the %s service %s method.",
					resultName, m.Service.Name, m.Name)
			}
			resultEx = m.Result.Example(design.Root.API.Random())
		}
		if len(m.Errors) > 0 {
			errors = make([]*ErrorInitData, len(m.Errors))
			for i, er := range m.Errors {
				errors[i] = buildErrorInitData(er, scope)
			}
		}
		desc = m.Description
		if desc == "" {
			desc = codegen.Goify(m.Name, true) + " implements " + m.Name + "."
		}
		return &MethodData{
			Name:        m.Name,
			VarName:     varName,
			Description: desc,
			Payload:     payloadName,
			PayloadDef:  payloadDef,
			PayloadRef:  payloadRef,
			PayloadDesc: payloadDesc,
			PayloadEx:   payloadEx,
			Result:      resultName,
			ResultDef:   resultDef,
			ResultRef:   resultRef,
			ResultDesc:  resultDesc,
			ResultEx:    resultEx,
			Errors:      errors,
		}
	}
}

func buildExpandedResultType(res *design.AttributeExpr, scope *codegen.NameScope) *design.AttributeExpr {
	var (
		seen map[string]struct{}
		// expanded result type will have the same structure as the source result type
		expType *design.AttributeExpr
	)
	{
		seen = make(map[string]struct{})
		expType = &design.AttributeExpr{Type: design.Dup(res.Type)}
	}
	buildExpandedTypeRecursive(res, expType, scope, seen)
	return expType
}

// buildExpandedTypeRecursive traverses the src attribute recursively and
// builds the tgt attribute identical to the src with all its user types
// replaced with expanded types.
func buildExpandedTypeRecursive(src, tgt *design.AttributeExpr, scope *codegen.NameScope, seen map[string]struct{}) {
	switch dt := src.Type.(type) {
	case design.UserType:
		if _, ok := seen[dt.Name()]; ok {
			// break the recursion
			return
		}
		seen[dt.Name()] = struct{}{}
		// Rename user type to expanded type
		newType := design.Dup(dt)
		ext := newType.(design.UserType)
		ext.Rename("Expanded" + codegen.Goify(dt.Name(), true))
		// Remove all validations from expanded type
		ext.Attribute().Validation = nil
		tgt.Type = ext
		buildExpandedTypeRecursive(dt.Attribute(), ext.Attribute(), scope, seen)
		if rt, ok := dt.(*design.ResultTypeExpr); ok && len(rt.Views) > 1 {
			// If src is a result type with more than one view, then add
			// a required view attribute to the expanded result type.
			if obj := design.AsObject(ext); obj != nil {
				obj.Set("view", &design.AttributeExpr{Type: design.String, Description: "View to render."})
				ext.Attribute().Validation = &design.ValidationExpr{Required: []string{"view"}}
			}
		}
	case *design.Array:
		tgtArr := tgt.Type.(*design.Array)
		buildExpandedTypeRecursive(dt.ElemType, tgtArr.ElemType, scope, seen)
	case *design.Map:
		tgtMap := tgt.Type.(*design.Map)
		buildExpandedTypeRecursive(dt.KeyType, tgtMap.KeyType, scope, seen)
		buildExpandedTypeRecursive(dt.ElemType, tgtMap.ElemType, scope, seen)
	case *design.Object:
		tgtObj := tgt.Type.(*design.Object)
		for _, n := range *dt {
			att := design.DupAtt(n.Attribute)
			buildExpandedTypeRecursive(n.Attribute, att, scope, seen)
			tgtObj.Set(n.Name, att)
		}
	}
}

// collectExpandedTypes collects all the expanded types from the given
// expanded result type exp.
func collectExpandedTypes(src, exp *design.AttributeExpr, scope *codegen.NameScope, seen map[string]struct{}) (data []*ExpandedTypeData) {
	collect := func(src, exp *design.AttributeExpr) []*ExpandedTypeData {
		return collectExpandedTypes(src, exp, scope, seen)
	}
	switch dt := src.Type.(type) {
	case design.UserType:
		if _, ok := seen[dt.Name()]; ok {
			// break the recursion
			return nil
		}
		seen[dt.Name()] = struct{}{}
		var (
			expName  string
			expRef   string
			expT     design.UserType
			validate string
		)
		expRef = scope.GoTypeRef(exp)
		expName = scope.GoTypeName(exp)
		expT = exp.Type.(design.UserType)
		t := &ExpandedTypeData{
			Name:     expName,
			VarName:  codegen.Goify(expName, true),
			Def:      scope.GoTypeDef(expT.Attribute(), false),
			Validate: validate,
			Ref:      expRef,
			Type:     expT,
			SrcType:  dt,
		}
		if rt, ok := dt.(*design.ResultTypeExpr); ok && len(rt.Views) > 1 {
			t.BuildRenderData(dt, expT, scope, "")
		} else {
			// It is not a result type. So build the validation code from src
			// since we have removed the required attributes from expanded type.
			t.Validate = codegen.RecursiveValidationCode(dt.(design.UserType).Attribute(), false, true, false, "e")
		}
		data = append(data, t)
		data = append(data, collect(dt.Attribute(), expT.Attribute())...)
	case *design.Array:
		expArr := exp.Type.(*design.Array)
		data = append(data, collect(dt.ElemType, expArr.ElemType)...)
	case *design.Map:
		expMap := exp.Type.(*design.Map)
		data = append(data, collect(dt.KeyType, expMap.KeyType)...)
		data = append(data, collect(dt.ElemType, expMap.ElemType)...)
	case *design.Object:
		expObj := exp.Type.(*design.Object)
		for _, n := range *dt {
			data = append(data, collect(n.Attribute, expObj.Attribute(n.Name))...)
		}
	}
	return
}

// BuildRenderData builds the render data which contain information on how to
// render an expanded result type based on a view. It also constructs the
// validation code for the expanded result type.
func (t *ExpandedTypeData) BuildRenderData(srcT, expT design.UserType, scope *codegen.NameScope, pkg string) {
	t.FullRef = scope.GoFullTypeRef(&design.AttributeExpr{Type: expT}, pkg)
	t.ResultName = scope.GoFullTypeName(&design.AttributeExpr{Type: srcT}, pkg)
	t.ResultRef = scope.GoFullTypeRef(&design.AttributeExpr{Type: srcT}, pkg)
	src, ok := srcT.(*design.ResultTypeExpr)
	if !ok {
		return
	}
	exp, ok := expT.(*design.ResultTypeExpr)
	if !ok {
		return
	}

	srcArr := design.AsArray(src)
	var (
		expTypeN  string
		expVar    string
		resVar    string
		expVarArr string
		resVarArr string
		srcVar    string
		tgtVar    string
		toPre     string
		fromPre   string
		helpers   []*codegen.TransformFunctionData
		valTgtVar string
		validate  string
		rds       []*RenderData
	)
	{
		expTypeN = exp.TypeName
		valTgtVar = "e"
		expVar = "e"
		resVar = "res"
		if srcArr != nil {
			t.IsCollection = true
			// src result type is a collection
			valTgtVar = "c"
			fromPre = fmt.Sprintf("%s := make(%s, len(%s))\nfor i, val := range %s {\n", expVar, scope.GoTypeRef(&design.AttributeExpr{Type: exp.Type}), resVar, resVar)
			toPre = fmt.Sprintf("%s := make(%s, len(%s))\nfor i, val := range %s {\n", resVar, t.ResultRef, expVar, expVar)
			validate = fmt.Sprintf("\nfor _, %s := range %s {\n", valTgtVar, expVar)
			expVarArr = "e[i]"
			resVarArr = "res[i]"
			expVar = "c"
			resVar = "c"
		}
	}

	if srcArr != nil {
		src = srcArr.ElemType.Type.(*design.ResultTypeExpr)
		if expArr := design.AsArray(exp); expArr != nil {
			exp = expArr.ElemType.Type.(*design.ResultTypeExpr)
		}
	}
	srcObj := design.AsObject(src)
	if srcObj == nil {
		// If result type is not an object type we can't build views
		return
	}
	expObj := design.AsObject(exp)

	validate += fmt.Sprintf("switch %s.View {", valTgtVar)
	for _, view := range src.Views {
		var (
			toCode   string
			fromCode string
		)
		toCode = toPre
		fromCode = fromPre

		// ut will contain all attributes found in the expanded result type.
		// which will be used in building the code for expanded result type
		// and validation.
		utObj := &design.Object{}
		ut := &design.UserTypeExpr{
			AttributeExpr: &design.AttributeExpr{Type: utObj},
			TypeName:      exp.TypeName,
		}

		// Iterate through the view attributes and build the conversion code for
		// expanded result type (to/from src result type). We iterate through
		// the view object twice - first iteration will populate ut with all attributes
		// that are a non-result type (we can then use GoTypeTransfrom to build
		// the code), second iteration will populate ut with the attributes of
		// type result types (so that we can use the to/from code generated with
		// these result types to build the code).
		vObj := view.Type.(*design.Object)
		for _, n := range *vObj {
			srcAtt := srcObj.Attribute(n.Name)
			if _, ok := srcAtt.Type.(*design.ResultTypeExpr); !ok {
				utObj.Set(n.Name, expObj.Attribute(n.Name))
			}
		}
		srcVar = expVar
		tgtVar = resVar
		if srcArr != nil {
			srcVar = "val"
		}
		code, hlprs, err := codegen.GoTypeTransform(ut, src.UserTypeExpr, srcVar, tgtVar, "", pkg, true, scope)
		if err != nil {
			panic(err) // bug
		}
		toCode += code
		helpers = codegen.AppendHelpers(helpers, hlprs)
		srcVar = resVar
		tgtVar = expVar
		if srcArr != nil {
			srcVar = "val"
		}
		code, hlprs, err = codegen.GoTypeTransform(src.UserTypeExpr, ut, srcVar, tgtVar, pkg, "", false, scope)
		if err != nil {
			panic(err) //bug
		}
		fromCode += code
		helpers = codegen.AppendHelpers(helpers, hlprs)

		// Second iteration: we only care about result types here
		vName := codegen.Goify(view.Name, true)
		for _, n := range *vObj {
			srcAtt := srcObj.Attribute(n.Name)
			if _, ok := srcAtt.Type.(*design.ResultTypeExpr); ok {
				expAtt := expObj.Attribute(n.Name)
				utObj.Set(n.Name, expAtt)
				// if the src attribute is a result type, then use the generated
				// to/from code to convert between the src type and expanded type.
				v := vName
				if attV, ok := n.Attribute.Metadata["view"]; ok {
					// if a view is explicitly set for the result type on the view attribute
					// use that view.
					v = codegen.Goify(attV[0], true)
				}
				varN := codegen.Goify(n.Name, true)
				tgtVar = expVar
				if srcArr != nil {
					tgtVar = "val"
				}
				toCode += fmt.Sprintf("\nif %s.%s != nil {\n%s.%s = %s.%s.to%s()\n}\n", tgtVar, varN, resVar, varN, tgtVar, varN, v)
				tgtVar = resVar
				if srcArr != nil {
					tgtVar = "val"
				}
				fromCode += fmt.Sprintf("\nif %s.%s != nil {\n%s.%s = new%s%s(%s.%s)\n}\n", tgtVar, varN, expVar, varN, expAtt.Type.Name(), v, tgtVar, varN)
			}
		}
		fromCode += fmt.Sprintf("\n%s.View = %q", expVar, view.Name)
		// Build the validations for the expanded result type as per
		// the src result type view.
		if src.Validation != nil {
			required := []string{}
			for _, n := range src.Validation.Required {
				if att := vObj.Attribute(n); att != nil {
					required = append(required, n)
				}
			}
			ut.Validation = src.Validation.Dup()
			ut.Validation.Required = required
			validate += fmt.Sprintf("\ncase %q:\n", view.Name)
			validate += codegen.RecursiveValidationCode(ut.Attribute(), false, true, false, valTgtVar)
		}

		if srcArr != nil {
			toCode += fmt.Sprintf("\n%s = %s", resVarArr, resVar)
			fromCode += fmt.Sprintf("\n%s = %s", expVarArr, expVar)
			// close array loop
			toCode += "}\n"
			fromCode += "}\n"

		}
		rds = append(rds, &RenderData{
			View:           view.Name,
			Description:    view.Description,
			ToResult:       "to" + vName,
			FromResult:     "new" + codegen.Goify(expTypeN, true) + vName,
			ToResultCode:   toCode,
			FromResultCode: fromCode,
			Ref:            scope.GoTypeRef(&design.AttributeExpr{Type: expT}),
		})
	}
	validate += "}\n" // close switch statement
	if srcArr != nil {
		validate += "}\n" // close array loop
	}
	t.Views = rds
	t.Validate = validate
	t.Helpers = helpers
}
