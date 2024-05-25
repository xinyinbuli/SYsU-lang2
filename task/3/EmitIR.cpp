#include "EmitIR.hpp"
#include <llvm/Transforms/Utils/ModuleUtils.h>

#define self (*this)

using namespace asg;

EmitIR::EmitIR(Obj::Mgr& mgr, llvm::LLVMContext& ctx, llvm::StringRef mid)
  : mMgr(mgr)
  , mMod(mid, ctx)
  , mCtx(ctx)
  , mIntTy(llvm::Type::getInt32Ty(ctx))
  , mCurIrb(std::make_unique<llvm::IRBuilder<>>(ctx))
  , mCtorTy(llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), false))
  , mCurFunc(nullptr)
{
}

llvm::Module&
EmitIR::operator()(asg::TranslationUnit* tu)
{
  for (auto&& i : tu->decls)
    self(i);
  return mMod;
}

//==============================================================================
// 类型
//==============================================================================

llvm::Type*
EmitIR::operator()(const Type* type)
{
  unsigned N;
  if (type->texp == nullptr) {
    switch (type->spec) {
      case Type::Spec::kInt:
        return llvm::Type::getInt32Ty(mCtx);      
      case Type::Spec::kVoid:
        return llvm::Type::getVoidTy(mCtx);
      case Type::Spec::kChar:
        N=8;
        return llvm::IntegerType::get(mCtx,8);
      case Type::Spec::kLong:
        return llvm::Type::getInt32Ty(mCtx);
      case Type::Spec::kLongLong:
        N=64;
        return llvm::IntegerType::get(mCtx,64);
      default:
        ABORT();
    }
  }

  Type subt;
  subt.spec = type->spec;
  subt.qual = type->qual;
  subt.texp = type->texp->sub;

  // TODO: 在此添加对指针类型、数组类型和函数类型的处理

  if (auto p = type->texp->dcst<FunctionType>()) {
    std::vector<llvm::Type*> pty;
    for(auto t:p->params){
      pty.push_back(self(t));
    }
    return llvm::FunctionType::get(self(&subt), std::move(pty), false);
  }
  if (auto p = type->texp->dcst<ArrayType>()) {
    return llvm::ArrayType::get(self(&subt), p->len);
  }
    if (auto p = type->texp->dcst<PointerType>()) {
    return llvm::PointerType::get(self(&subt),0);
  }
  ABORT();
}

//==============================================================================
// 表达式
//==============================================================================

llvm::Value*
EmitIR::operator()(Expr* obj)
{
  // TODO: 在此添加对更多表达式处理的跳转
  if (auto p = obj->dcst<IntegerLiteral>())
    return self(p);

  if (auto p = obj->dcst<StringLiteral>())
    return self(p);

  if (auto p = obj->dcst<DeclRefExpr>())
    return self(p);

  if (auto p = obj->dcst<ParenExpr>())
    return self(p);

  if (auto p = obj->dcst<UnaryExpr>())
    return self(p);

  if (auto p = obj->dcst<ImplicitCastExpr>())
    return self(p);

  if (auto p = obj->dcst<BinaryExpr>())
    return self(p);
  
  if (auto p = obj->dcst<ImplicitInitExpr>())
    return self(p);

  if (auto p = obj->dcst<CallExpr>())
    return self(p);
    
  if (auto p = obj->dcst<InitListExpr>()){
    return self(p);
  }


  ABORT();
}

llvm::Constant*
EmitIR::operator()(IntegerLiteral* obj)
{
  return llvm::ConstantInt::get(self(obj->type), obj->val);
}

llvm::Constant*
EmitIR::operator()(StringLiteral* obj)
{
  return llvm::ConstantDataArray::getString(mCtx, obj->val);
}


llvm::Value* EmitIR::operator()(CallExpr* obj)
{
    // 获取函数名
    auto p = obj->dcst<StringLiteral>();
    // 获取函数
    llvm::Function *func = mMod.getFunction(p->val);

    // 准备函数参数
    std::vector<llvm::Value *> args;
    for (auto arg : obj->args) {
        args.push_back(self(arg));
    }

    // 创建函数调用指令并返回结果
    return mCurIrb->CreateCall(func, args);
}



llvm::Value*
EmitIR::operator()(UnaryExpr* obj)
{
  switch (obj->op){
    case asg::UnaryExpr::kNeg:
      return mCurIrb->CreateNeg(self(obj->sub));
    case asg::UnaryExpr::kPos:
      return self(obj->sub);
    case asg::UnaryExpr::kNot:
      return mCurIrb->CreateNot(self(obj->sub));
    default:
      ABORT();
  }
}


llvm::Value*
EmitIR::operator()(ParenExpr* obj)
{
  return self(obj->sub);
}

llvm::Value*
EmitIR::operator()(BinaryExpr* obj)
{
  llvm::Value *lftVal, *rhtVal;

  lftVal = self(obj->lft);

  auto& irb = *mCurIrb;
  rhtVal = self(obj->rht);
  std::vector<llvm::Value *> idxList{irb.getInt64(0), rhtVal};
  switch (obj->op) {
    case BinaryExpr::kAdd:
      return irb.CreateAdd(lftVal, rhtVal);
    case BinaryExpr::kMul:
      return irb.CreateMul(lftVal, rhtVal);
    case BinaryExpr::kDiv:
      return irb.CreateSDiv(lftVal, rhtVal);
    case BinaryExpr::kMod:
      return irb.CreateSRem(lftVal, rhtVal);
    case BinaryExpr::kSub:
      return irb.CreateSub(lftVal, rhtVal);
    case BinaryExpr::kGt:
      return irb.CreateICmpSGT(lftVal, rhtVal);
    case BinaryExpr::kLt:
      return irb.CreateICmpSLT(lftVal, rhtVal);
    case BinaryExpr::kGe:
      return irb.CreateICmpSGE(lftVal, rhtVal);
    case BinaryExpr::kLe:
      return irb.CreateICmpSLE(lftVal, rhtVal);
    case BinaryExpr::kEq:
      return irb.CreateICmpEQ(lftVal, rhtVal);
    case BinaryExpr::kNe:
      return irb.CreateICmpNE(lftVal, rhtVal);  
    case BinaryExpr::kAssign:
      irb.CreateStore(rhtVal, lftVal);
      return rhtVal; 
    case BinaryExpr::kIndex:
      return irb.CreateInBoundsGEP(lftVal->getType(), lftVal, idxList);
    default:
      ABORT();
  }
}

llvm::Value*
EmitIR::operator()(ImplicitCastExpr* obj)
{
  auto sub = self(obj->sub);

  auto& irb = *mCurIrb;
  switch (obj->kind) {
    case ImplicitCastExpr::kLValueToRValue: {
      auto ty = self(obj->sub->type);
      auto loadVal = irb.CreateLoad(ty, sub);
      return loadVal;
    }
    case ImplicitCastExpr::kArrayToPointerDecay: {
      auto ty = self(obj->sub->type);
      auto loadVal = irb.CreateLoad(ty, sub);
      return loadVal;
    }
    case ImplicitCastExpr::kFunctionToPointerDecay: {
      auto ty = self(obj->sub->type);
      auto loadVal = irb.CreateLoad(ty, sub);
      return loadVal;
    }
    default:
      ABORT();
  }
}

llvm::Value*
EmitIR::operator()(InitListExpr* obj)
{
    auto val = (llvm::AllocaInst*)obj->any; // 将 obj->any 的类型更改为 llvm::AllocaInst*
    auto& irb = *mCurIrb;
    auto elementType = val->getAllocatedType(); // 获取元素类型

    // 创建一个指向数组的指针
    auto arrayPtr = irb.CreatePointerCast(val, llvm::PointerType::get(elementType, 0));
 
    // 迭代初始化列表中的每个元素
    for (auto l : obj->list) {
        if (auto p = l->dcst<IntegerLiteral>()) {
            auto initVal = llvm::ConstantInt::get(elementType, p->val);
            irb.CreateStore(initVal, arrayPtr); // 将初始化值存储到数组
            arrayPtr = irb.CreateGEP(elementType, arrayPtr, irb.getInt32(1)); // 移动到下一个数组元素的地址
        }
        else if(auto p = l->dcst<ImplicitCastExpr>()) {
            auto initVal = self(p);
            irb.CreateStore(initVal, arrayPtr); // 将初始化值存储到数组
            arrayPtr = irb.CreateGEP(elementType, arrayPtr, irb.getInt32(1)); // 移动到下一个数组元素的地址
        }
    }

    return arrayPtr; // 返回最后一个数组元素的地址
}


llvm::Value*
EmitIR::operator()(DeclRefExpr* obj)
{
  // 在LLVM IR层面，左值体现为返回指向值的指针
  // 在ImplicitCastExpr::kLValueToRValue中发射load指令从而变成右值
  return reinterpret_cast<llvm::Value*>(obj->decl->any);
}

// TODO: 在此添加对更多表达式类型的处理

//==============================================================================
// 语句
//==============================================================================

void
EmitIR::operator()(Stmt* obj)
{
  // TODO: 在此添加对更多Stmt类型的处理的跳转

  if (auto p = obj->dcst<CompoundStmt>())
    return self(p);

  if (auto p = obj->dcst<ReturnStmt>())
    return self(p);

  if (auto p = obj->dcst<DeclStmt>())
    return self(p);

  if (auto p = obj->dcst<NullStmt>())
    return self(p);

  if (auto p = obj->dcst<ExprStmt>())
    return self(p);
  
  ABORT();
}

// TODO: 在此添加对更多Stmt类型的处理
void
EmitIR::operator()(ExprStmt* obj)
{
    self(obj->expr);
}

void
EmitIR::operator()(CompoundStmt* obj)
{
  // TODO: 可以在此添加对符号重名的处理
    for (auto&& stmt : obj->subs)
    self(stmt);
}

void
EmitIR::operator()(ReturnStmt* obj)
{
  auto& irb = *mCurIrb;

  llvm::Value* retVal;
  if (!obj->expr)
    retVal = nullptr;
  else
    retVal = self(obj->expr);

  mCurIrb->CreateRet(retVal);

  auto exitBb = llvm::BasicBlock::Create(mCtx, "return_exit", mCurFunc);
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(exitBb);
}

void
EmitIR::operator()(DeclStmt* obj)
{
  for (auto&& decl : obj->decls)
    self(decl);
}

//==============================================================================
// 声明
//==============================================================================

void
EmitIR::operator()(Decl* obj)
{
   // TODO: 添加变量声明处理的跳转
  if (auto p = obj->dcst<VarDecl>())
    return self(p);

  if (auto p = obj->dcst<FunctionDecl>())
    return self(p);

  ABORT();
}

void
EmitIR::operator()(FunctionDecl* obj)
{
  // 创建函数
  auto fty = llvm::dyn_cast<llvm::FunctionType>(self(obj->type));
  auto func = llvm::Function::Create(
    fty, llvm::GlobalVariable::ExternalLinkage, obj->name, mMod);

  obj->any = func;

  if (obj->body == nullptr)
    return;
  auto entryBb = llvm::BasicBlock::Create(mCtx, "entry", func);
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(entryBb);
  auto& entryIrb = *mCurIrb;

  // TODO: 添加对函数参数的处理
  auto argIter = func->arg_begin();
  for(auto p:obj->params){
    argIter++->setName(p->name); 
  }

  // 翻译函数体
  mCurFunc = func;
  self(obj->body);
  auto& exitIrb = *mCurIrb;

  if (fty->getReturnType()->isVoidTy())
    exitIrb.CreateRetVoid();
  else
    exitIrb.CreateUnreachable();
}

void
EmitIR::trans_init(llvm::Value* val, Expr* obj)
{
  auto& irb = *mCurIrb;

  // 仅处理整数字面量的初始化
  if (auto p = obj->dcst<IntegerLiteral>()) {
    auto initVal = llvm::ConstantInt::get(self(p->type), p->val);
    irb.CreateStore(initVal, val);
    return;
  }

  if (auto p = obj->dcst<InitListExpr>()) {
    p->any=val;
    self(p);
    return;
  }
  // 如果表达式不是整数字面量，则中断编译
  ABORT();
}

void
EmitIR::operator()(VarDecl* obj)
{
  if(mCurFunc==nullptr)
  {    
    auto ty = self(obj->type); 
    auto gvar = new llvm::GlobalVariable(
      mMod, ty, false, llvm::GlobalVariable::ExternalLinkage, nullptr, obj->name);

    obj->any = gvar;

    // 默认初始化为 0
    gvar->setInitializer(llvm::Constant::getNullValue(ty));

    if (obj->init == nullptr)
      return;

    // 创建构造函数用于初始化
    mCurFunc = llvm::Function::Create(
      mCtorTy, llvm::GlobalVariable::PrivateLinkage, "ctor_" + obj->name, mMod);
    llvm::appendToGlobalCtors(mMod, mCurFunc, 65535);

    auto entryBb = llvm::BasicBlock::Create(mCtx, "entry", mCurFunc);
    mCurIrb = std::make_unique<llvm::IRBuilder<>>(entryBb);
    trans_init(gvar, obj->init);
    mCurIrb->CreateRet(nullptr);
    mCurFunc=nullptr;
  }
  else{
    auto ty = self(obj->type); 
    auto a=mCurIrb->CreateAlloca(ty,nullptr,obj->name);
    obj->any = a;
    if (obj->init == nullptr)
    return;
    trans_init(a, obj->init);
  }
}