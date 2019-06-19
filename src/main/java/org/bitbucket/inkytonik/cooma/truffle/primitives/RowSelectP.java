package org.bitbucket.inkytonik.cooma.truffle.primitives;

import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.FieldValueRuntime;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RowRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

import java.util.Arrays;
import java.util.Optional;

public class RowSelectP extends Primitive {

    @Override
    public Integer getNumargs() {
        return 2;
    }

    @Override
    public RuntimeValue run(Rho rho, String[] xs) throws Exception {

        String rowId = xs[0];
        String fieldId = xs[1];

        RuntimeValue row = rho.get(rowId);
        if (row instanceof RowRuntimeValue){
            Optional<FieldValueRuntime> opV = Arrays.stream(((RowRuntimeValue) row).getFields())
                    .filter( (FieldValueRuntime fieldR) -> fieldR.getX().equals(fieldId) )
                    .findFirst();

            if (opV.isPresent()){
                return opV.get().getV();
            }else{
                //TODO: Fix this exception, avoid using generic exceptions
                throw new Exception(String.format("%s: can't find field %s in %s", getShow(), fieldId, Arrays.toString(((RowRuntimeValue) row).getFields())));
            }
        }

        //TODO: add the cases for ErrR and when other thing is found.

        /*
        def run(interp : Interpreter)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR = {
            val Vector(r, f1) = xs

            interp.lookupR(rho, r) match {
                case interp.RowR(fields) =>
                    fields.collectFirst {
                        case interp.FldR(f2, v) if f1 == f2 =>
                            v
                    } match {
                        case Some(v) =>
                            v
                        case None =>
                            sys.error(s"$show: can't find field $f1 in $fields")
                    }

                case err : interp.ErrR =>
                    err

                case v =>
                    sys.error(s"$show: $r is $v, looking for field $f1")
            }
        }
         */

        return null;
    }

    @Override
    public String getShow() {
        return "select";
    }
}

