/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2026  Bill Hails
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include "minlam_cpsTrampoline.h"

#include "common.h"
#include "memory.h"
#include "minlam_cps.h"

static CpsWork *invokeWork(CpsKont *k, MinExp *arg) {
    return k->wrapper(arg, k->env);
}

static CpsWork *cpsStep(CpsWork *work) {
    switch (work->type) {
    case CPSWORK_TYPE_RESULT:
        return work;

    case CPSWORK_TYPE_TCROOT: {
        CpsTcRootThunk *tcRoot = getCpsWork_TcRoot(work);
        return makeCpsWork_Tc(tcRoot->exp, tcRoot->cont0);
    }

    case CPSWORK_TYPE_TK:
    case CPSWORK_TYPE_TSK:
    case CPSWORK_TYPE_TKIFFAFTERCONDITION:
    case CPSWORK_TYPE_TKIFFAFTERCONSEQUENT:
    case CPSWORK_TYPE_TKIFFBUILD:
    case CPSWORK_TYPE_TKAMBAFTERLEFT:
    case CPSWORK_TYPE_TKAMBBUILD:
    case CPSWORK_TYPE_TKLETRECAFTERBODY:
    case CPSWORK_TYPE_TKMAPINTCONDCASES:
    case CPSWORK_TYPE_TKMAPINTCONDCASESAFTERBODY:
    case CPSWORK_TYPE_TKMAPCHARCONDCASES:
    case CPSWORK_TYPE_TKMAPCHARCONDCASESAFTERBODY:
    case CPSWORK_TYPE_TKMAPMATCHCASES:
    case CPSWORK_TYPE_TKMAPMATCHCASESAFTERBODY:
    case CPSWORK_TYPE_CPSMAPBINDINGS:
    case CPSWORK_TYPE_CPSMAPBINDINGSAFTERVAL:
        return cpsStepTk(work);

    case CPSWORK_TYPE_TC:
    case CPSWORK_TYPE_TCIFFAFTERCONDITION:
    case CPSWORK_TYPE_TCIFFAFTERTHEN:
    case CPSWORK_TYPE_TCIFFBUILD:
    case CPSWORK_TYPE_TCAMBAFTERLEFT:
    case CPSWORK_TYPE_TCAMBBUILD:
    case CPSWORK_TYPE_TCLETRECAFTERBODY:
    case CPSWORK_TYPE_MLAMAFTERBODY:
    case CPSWORK_TYPE_TCMAPINTCONDCASES:
    case CPSWORK_TYPE_TCMAPINTCONDCASESAFTERBODY:
    case CPSWORK_TYPE_TCMAPCHARCONDCASES:
    case CPSWORK_TYPE_TCMAPCHARCONDCASESAFTERBODY:
    case CPSWORK_TYPE_TCMAPMATCHCASES:
    case CPSWORK_TYPE_TCMAPMATCHCASESAFTERBODY:
        return cpsStepTc(work);

    case CPSWORK_TYPE_INVOKE: {
        CpsInvokeThunk *invoke = getCpsWork_Invoke(work);
        return invokeWork(invoke->kont, invoke->arg);
    }

    default:
        cant_happen("unhandled CpsWork tag %s in cpsStep",
                    cpsWorkTypeName(work->type));
        return NULL;
    }
}

MinExp *runCpsWorkToResult(CpsWork *work) {
    int save = PROTECT(work);

    while (work->type != CPSWORK_TYPE_RESULT) {
        CpsWork *next = cpsStep(work);
        REPLACE_PROTECT(save, next);
        work = next;
    }

    MinExp *result = getCpsWork_Result(work);
    UNPROTECT(save);
    return result;
}

MinExp *runCpsTrampolineTc(MinExp *rootExp, MinExp *cont0) {
    CpsWork *work = makeCpsWork_TcRoot(rootExp, cont0);
    return runCpsWorkToResult(work);
}
