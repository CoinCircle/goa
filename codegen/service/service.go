package service

import (
	"fmt"
	"path/filepath"

	"goa.design/goa/codegen"
	"goa.design/goa/design"
)

// File returns the service file for the given service.
func File(service *design.ServiceExpr) *codegen.File {
	path := filepath.Join(codegen.Gendir, codegen.SnakeCase(service.Name), "service.go")
	svc := Services.Get(service.Name)
	header := codegen.Header(
		service.Name+" service",
		svc.PkgName,
		[]*codegen.ImportSpec{
			{Path: "context"},
			{Path: "unicode/utf8"},
			{Path: "goa.design/goa"},
		})
	def := &codegen.SectionTemplate{Name: "service", Source: serviceT, Data: svc}

	sections := []*codegen.SectionTemplate{header, def}
	seen := make(map[string]struct{})

	for _, m := range svc.Methods {
		if m.PayloadDef != "" {
			if _, ok := seen[m.Payload]; !ok {
				seen[m.Payload] = struct{}{}
				sections = append(sections, &codegen.SectionTemplate{
					Name:   "service-payload",
					Source: payloadT,
					Data:   m,
				})
			}
		}
		if m.ResultDef != "" {
			if _, ok := seen[m.Result]; !ok {
				seen[m.Result] = struct{}{}
				sections = append(sections, &codegen.SectionTemplate{
					Name:   "service-result",
					Source: resultT,
					Data:   m,
				})
			}
		}
	}

	for _, ut := range svc.UserTypes {
		if _, ok := seen[ut.Name]; !ok {
			sections = append(sections, &codegen.SectionTemplate{
				Name:   "service-user-type",
				Source: userTypeT,
				Data:   ut,
			})
		}
	}

	for _, t := range svc.ExpandedTypes {
		sections = append(sections, &codegen.SectionTemplate{
			Name:   "expanded-type",
			Source: userTypeT,
			Data:   t,
		})
	}

	var errorTypes []*UserTypeData
	for _, et := range svc.ErrorTypes {
		if et.Type == design.ErrorResult {
			continue
		}
		if _, ok := seen[et.Name]; !ok {
			sections = append(sections, &codegen.SectionTemplate{
				Name:   "error-user-type",
				Source: userTypeT,
				Data:   et,
			})
			errorTypes = append(errorTypes, et)
		}
	}

	for _, et := range errorTypes {
		if et.Type == design.ErrorResult {
			continue
		}
		sections = append(sections, &codegen.SectionTemplate{
			Name:    "service-error",
			Source:  errorT,
			FuncMap: map[string]interface{}{"errorName": errorName},
			Data:    et,
		})
	}
	for _, er := range svc.ErrorInits {
		sections = append(sections, &codegen.SectionTemplate{
			Name:   "error-init-func",
			Source: errorInitT,
			Data:   er,
		})
	}

	for _, t := range svc.ExpandedTypes {
		sections = append(sections, &codegen.SectionTemplate{
			Name:   "expanded-type-init",
			Source: expandedTypeInitT,
			Data:   t,
		})
	}
	for _, t := range svc.ExpandedTypes {
		sections = append(sections, &codegen.SectionTemplate{
			Name:   "expanded-type-validate",
			Source: validateT,
			Data:   t,
		})
	}
	for _, h := range svc.Helpers {
		sections = append(sections, &codegen.SectionTemplate{
			Name:   "transform-helper",
			Source: transformHelperT,
			Data:   h,
		})
	}
	return &codegen.File{Path: path, SectionTemplates: sections}
}

func errorName(et *UserTypeData) string {
	obj := design.AsObject(et.Type)
	if obj != nil {
		for _, att := range *obj {
			if _, ok := att.Attribute.Metadata["struct:error:name"]; ok {
				return fmt.Sprintf("e.%s", codegen.Goify(att.Name, true))
			}
		}
	}
	return fmt.Sprintf("%q", et.Name)
}

// serviceT is the template used to write an service definition.
const serviceT = `
{{ comment .Description }}
type Service interface {
{{- range .Methods }}
	{{ comment .Description }}
	{{- if .ExpandedResult }}
		{{ comment "It must return one of the following views" }}
		{{- range .ExpandedResult.Views }}
			{{- if .Description }}
			{{ printf "* %s: %s" .View .Description | comment }}
			{{- else }}
			{{ printf "* %s" .View | comment }}
			{{- end }}
		{{- end }}
	{{- end }}
	{{ .VarName }}(context.Context{{ if .Payload }}, {{ .PayloadRef }}{{ end }}) {{ if .Result }}({{ .ResultRef }}, {{ if .ExpandedResult }}string, {{ end }}error){{ else }}error{{ end }}
{{- end }}
}

// ServiceName is the name of the service as defined in the design. This is the
// same value that is set in the endpoint request contexts under the ServiceKey
// key.
const ServiceName = {{ printf "%q" .Name }}

// MethodNames lists the service method names as defined in the design. These
// are the same values that are set in the endpoint request contexts under the
// MethodKey key.
var MethodNames = [{{ len .Methods }}]string{ {{ range .Methods }}{{ printf "%q" .Name }}, {{ end }} }
`

const payloadT = `{{ comment .PayloadDesc }}
type {{ .Payload }} {{ .PayloadDef }}
`

const resultT = `{{ comment .ResultDesc }}
type {{ .Result }} {{ .ResultDef }}
`

const userTypeT = `{{ comment .Description }}
type {{ .VarName }} {{ .Def }}
`

const errorT = `// Error returns an error description.
func (e {{ .Ref }}) Error() string {
	return {{ printf "%q" .Description }}
}

// ErrorName returns {{ printf "%q" .Name }}.
func (e {{ .Ref }}) ErrorName() string {
	return {{ errorName . }}
}
`

// input: map[string]{"Type": TypeData, "Error": ErrorData}
const errorInitT = `{{ printf "%s builds a %s from an error." .Name .TypeName |  comment }}
func {{ .Name }}(err error) {{ .TypeRef }} {
	return &{{ .TypeName }}{
		Name: {{ printf "%q" .ErrName }},
		ID: goa.NewErrorID(),
		Message: err.Error(),
	{{- if .Temporary }}
		Temporary: true,
	{{- end }}
	{{- if .Timeout }}
		Timeout: true,
	{{- end }}
	}
}
`

// input: ExpandedTypeData
const validateT = `{{ printf "Validate runs the validations defined on %s." .VarName | comment }}
func (e {{ .Ref }}) Validate() (err error) {
	{{ .Validate }}
	return
}
`

// input: ExpandedTypeData
const expandedTypeInitT = `{{- range .Views }}
{{ printf "%s converts %s result type to %s type using the %s view." .FromResult $.ResultName $.Name .View | comment }}
func {{ .FromResult }}(res {{ $.ResultRef }}) {{ $.Ref }} {
	{{ .FromResultCode }}
	return e
}
{{ end }}
`
